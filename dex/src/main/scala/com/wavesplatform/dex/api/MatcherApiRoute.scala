package com.wavesplatform.dex.api

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.FutureDirectives
import akka.pattern.{AskTimeoutException, ask}
import akka.stream.Materializer
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.dex.AddressActor.OrderListType
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex._
import com.wavesplatform.dex.api.http._
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.transaction.ExchangeTransactionV2
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.market.MatcherActor._
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.{MatcherSettings, OrderFeeSettings}
import com.wavesplatform.dex.time.Time
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.Success

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(assetPairBuilder: AssetPairBuilder,
                           matcherPublicKey: PublicKey,
                           matcher: ActorRef,
                           addressActor: ActorRef,
                           storeEvent: StoreEvent,
                           orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                           orderBookHttpInfo: OrderBookHttpInfo,
                           getActualTickSize: AssetPair => BigDecimal,
                           orderValidator: Order => FutureResult[Order],
                           matcherSettings: MatcherSettings,
                           matcherStatus: () => Matcher.Status,
                           db: DB,
                           time: Time,
                           currentOffset: () => QueueEventWithMeta.Offset,
                           lastOffset: () => Future[QueueEventWithMeta.Offset],
                           matcherAccountFee: Long,
                           apiKeyHash: Option[Array[Byte]],
                           rateCache: RateCache,
                           validatedAllowedOrderVersions: () => Future[Set[Byte]],
                           getActualOrderFeeSettings: () => OrderFeeSettings)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with HasStatusBarrier
    with ScorexLogging {

  import PathMatchers._

  private implicit val executionContext: ExecutionContext = mat.executionContext
  private implicit val timeout: Timeout                   = matcherSettings.actorResponseTimeout

  private type LogicResponseHandler = PartialFunction[Any, ToResponseMarshallable]

  private val timer      = Kamon.timer("matcher.api-requests")
  private val placeTimer = timer.refine("action" -> "place")

  private def invalidJsonResponse(fields: List[String] = Nil): StandardRoute = complete { InvalidJsonResponse(error.InvalidJson(fields)) }
  private val invalidUserPublicKey: StandardRoute                            = complete { SimpleErrorResponse(StatusCodes.Forbidden, error.UserPublicKeyIsNotValid) }

  private val invalidJsonParsingRejectionsHandler =
    server.RejectionHandler
      .newBuilder()
      .handle {
        case ValidationRejection(_, Some(e: PlayJsonException)) => invalidJsonResponse(e.errors.map(_._1.toString).toList)
        case _: UnsupportedRequestContentTypeRejection          => invalidJsonResponse()
      }
      .result()

  private val gRPCExceptionsHandler: ExceptionHandler = ExceptionHandler {
    case ex: WavesNodeConnectionLostException =>
      log.error("Waves Node connection lost", ex)
      complete { WavesNodeUnavailable(error.WavesNodeConnectionBroken) }
    case ex =>
      log.error("An unexpected error occurred", ex)
      complete { WavesNodeUnavailable(error.UnexpectedError) }
  }

  override lazy val route: Route = pathPrefix("matcher") {
    getMatcherPublicKey ~ orderBookInfo ~ getSettings ~ getRates ~ getCurrentOffset ~ getLastOffset ~
      getOldestSnapshotOffset ~ getAllSnapshotOffsets ~
      matcherStatusBarrier {
        handleExceptions(gRPCExceptionsHandler) {
          handleRejections(invalidJsonParsingRejectionsHandler) {
            getOrderBook ~ marketStatus ~ placeLimitOrder ~ placeMarketOrder ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
              getAllOrderHistory ~ tradableBalance ~ reservedBalance ~ orderStatus ~ getOrderStatusInfoByIdWithApiKey ~ getOrderStatusInfoByIdWithSignature ~
              historyDelete ~ cancel ~ cancelAll ~ cancelAllById ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder ~
              upsertRate ~ deleteRate ~ saveSnapshots
          }
        }
      }
  }

  private def wrapMessage(message: String): JsObject = Json.obj("message" -> message)

  private def unavailableOrderBookBarrier(p: AssetPair): Directive0 = orderBook(p) match {
    case Some(x) => if (x.isRight) pass else complete(OrderBookUnavailable(error.OrderBookBroken(p)))
    case None    => forceCheckOrderBook(p)
  }

  private def forceCheckOrderBook(p: AssetPair): Directive0 = onComplete(matcher ? ForceStartOrderBook(p)).flatMap {
    case Success(_) => pass
    case _          => complete(OrderBookUnavailable(error.OrderBookBroken(p)))
  }

  private def withAssetPair(p: AssetPair,
                            redirectToInverse: Boolean = false,
                            suffix: String = "",
                            formatError: MatcherError => ToResponseMarshallable = InfoNotFound.apply): Directive1[AssetPair] = {
    FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p).value } flatMap {
      case Right(_) => provide(p)
      case Left(e) if redirectToInverse =>
        FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p.reverse).value } flatMap {
          case Right(_) => redirect(s"/matcher/orderbook/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
          case Left(_)  => complete { formatError(e) }
        }
      case Left(e) => complete { formatError(e) }
    }
  }

  private def withAsset(a: Asset): Directive1[Asset] = {
    FutureDirectives.onSuccess { assetPairBuilder.validateAssetId(a).value } flatMap {
      case Right(_) => provide(a)
      case Left(e)  => complete { InfoNotFound(e) }
    }
  }

  private def withCancelRequest(f: CancelOrderRequest => Route): Route =
    post {
      entity(as[CancelOrderRequest]) { req =>
        if (req.isSignatureValid()) f(req) else complete(InvalidSignature)
      } ~ complete(StatusCodes.BadRequest)
    } ~ complete(StatusCodes.MethodNotAllowed)

  private def placeOrder(endpoint: PathMatcher[Unit], isMarket: Boolean): Route = path(endpoint) {
    (pathEndOrSingleSlash & entity(as[Order])) { order =>
      withAssetPair(order.assetPair, formatError = e => StatusCodes.BadRequest -> ApiError.from(e, "OrderRejected")) { pair =>
        unavailableOrderBookBarrier(pair) {
          complete(
            placeTimer.measureFuture {
              orderValidator(order).value flatMap {
                case Right(o) =>
                  placeTimer.measureFuture {
                    askAddressActor(order.sender, AddressActor.Command.PlaceOrder(o, isMarket)) {
                      case AddressActor.Event.OrderAccepted(x) => SimpleResponse(ApiSuccessfulPlace(x))
                      case x: error.MatcherError =>
                        if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> ApiError.from(x, "WavesNodeUnavailable")
                        else StatusCodes.BadRequest                                       -> ApiError.from(x, "OrderRejected")
                    }
                  }
                case Left(e) => Future.successful[ToResponseMarshallable] { StatusCodes.BadRequest -> ApiError.from(e, "OrderRejected") }
              }
            }
          )
        }
      }
    }
  }

  private def signedGet(publicKey: PublicKey): Directive0 =
    (headerValueByName("Timestamp") & headerValueByName("Signature")).tflatMap {
      case (timestamp, sig) =>
        Base58.tryDecodeWithLimit(sig).map(crypto.verify(_, publicKey ++ Longs.toByteArray(timestamp.toLong), publicKey)) match {
          case Success(true) => pass
          case _             => complete(InvalidSignature)
        }
    }

  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET", response = classOf[String])
  def getMatcherPublicKey: Route = (pathEndOrSingleSlash & get) {
    complete { ApiMatcherPublicKey(matcherPublicKey) }
  }

  @Path("/settings")
  @ApiOperation(value = "Matcher Settings", notes = "Get matcher settings", httpMethod = "GET", response = classOf[ApiMatcherPublicSettings])
  def getSettings: Route = (path("settings") & get) {
    complete(
      validatedAllowedOrderVersions() map { allowedOrderVersions =>
        SimpleResponse(
          ApiMatcherPublicSettings(
            matcherPublicKey = matcherPublicKey,
            matcherVersion = Version.VersionString,
            priceAssets = matcherSettings.priceAssets,
            orderFee = ApiMatcherPublicSettings.ApiOrderFeeSettings.fromSettings(
              settings = getActualOrderFeeSettings(),
              matcherAccountFee = matcherAccountFee,
              allRates = rateCache.getAllRates
            ),
            orderVersions = allowedOrderVersions.toSeq.sorted,
            networkByte = matcherSettings.addressSchemeCharacter.toInt
          )
        )
      }
    )
  }

  @Path("/settings/rates")
  @ApiOperation(
    value = "Asset rates",
    notes = "Get current rates of assets (price of 1 Waves in the specified asset)",
    httpMethod = "GET",
    response = classOf[Map[Asset, Double]]
  )
  def getRates: Route = (path("settings" / "rates") & get) {
    complete { ApiRates(rateCache.getAllRates) }
  }

  @Path("/settings/rates/{assetId}")
  @ApiOperation(
    value = "Add or update rate for the specified asset",
    httpMethod = "PUT",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Option[Double]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is added or updated", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "rate",
                           value = "Rate associated with the specified asset",
                           dataType = "double",
                           paramType = "body",
                           required = true)
    )
  )
  def upsertRate: Route = {
    (path("settings" / "rates" / AssetPM) & put & withAuth) { a =>
      entity(as[Double]) { rate =>
        if (rate <= 0) complete { RateError(error.NonPositiveAssetRate) } else
          withAsset(a) { asset =>
            complete(
              if (asset == Waves) RateError(error.WavesImmutableRate)
              else {
                val assetStr = asset.toString
                rateCache.upsertRate(asset, rate) match {
                  case None     => StatusCodes.Created -> wrapMessage(s"The rate $rate for the asset $assetStr added")
                  case Some(pv) => StatusCodes.OK      -> wrapMessage(s"The rate for the asset $assetStr updated, old value = $pv, new value = $rate")
                }
              }
            )
          }
      }
    }
  }

  @Path("/settings/rates/{assetId}")
  @ApiOperation(
    value = "Delete rate for the specified asset",
    httpMethod = "DELETE",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Option[Double]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is deleted", dataType = "string", paramType = "path")
    )
  )
  def deleteRate: Route = (path("settings" / "rates" / AssetPM) & delete & withAuth) { a =>
    withAsset(a) { asset =>
      complete(
        if (asset == Waves) RateError(error.WavesImmutableRate)
        else {
          val assetStr = asset.toString
          rateCache.deleteRate(asset) match {
            case None     => RateError(error.RateNotFound(asset), StatusCodes.NotFound)
            case Some(pv) => StatusCodes.OK -> wrapMessage(s"The rate for the asset $assetStr deleted, old value = $pv")
          }
        }
      )
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(
    value = "Get Order Book for a given Asset Pair",
    notes = "Get Order Book for a given Asset Pair",
    httpMethod = "GET",
    response = classOf[ApiV0OrderBook]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "depth",
                           value = "Limit the number of bid/ask records returned",
                           required = false,
                           dataType = "integer",
                           paramType = "query")
    )
  )
  def getOrderBook: Route = (path("orderbook" / AssetPairPM) & get) { p =>
    parameters('depth.as[Int].?) { depth =>
      withAssetPair(p, redirectToInverse = true, depth.fold("")(d => s"?depth=$d")) { pair =>
        complete { orderBookHttpInfo.getHttpView(pair, MatcherModel.Normalized, depth) }
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/status")
  @ApiOperation(
    value = "Get Market Status",
    notes = "Get current market data such as last trade, best bid and ask",
    httpMethod = "GET",
    response = classOf[ApiMarketStatus]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def marketStatus: Route = (path("orderbook" / AssetPairPM / "status") & get) { p =>
    withAssetPair(p, redirectToInverse = true, suffix = "/status") { pair =>
      complete { orderBookHttpInfo.getMarketStatus(pair) }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/info")
  @ApiOperation(value = "Get order restrictions for the specified asset pair", httpMethod = "GET", response = classOf[ApiOrderBookInfo])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def orderBookInfo: Route = (path("orderbook" / AssetPairPM / "info") & get) { p =>
    withAssetPair(p, redirectToInverse = true, suffix = "/info") { pair =>
      complete { SimpleResponse(orderBookInfo(pair)) }
    }
  }

  private def orderBookInfo(pair: AssetPair) = ApiOrderBookInfo(
    restrictions = matcherSettings.orderRestrictions.get(pair),
    matchingRules = ApiOrderBookInfo.MatchingRuleSettings(tickSize = getActualTickSize(pair).toDouble)
  )

  @Path("/orderbook")
  @ApiOperation(
    value = "Place order",
    notes = "Place a new limit order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    response = classOf[ApiSuccessfulPlace]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.domain.order.Order"
      )
    )
  )
  def placeLimitOrder: Route = placeOrder("orderbook", isMarket = false)

  @Path("/orderbook/market")
  @ApiOperation(
    value = "Place market order",
    notes = "Place a new market order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    response = classOf[ApiSuccessfulPlace]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.domain.order.Order"
      )
    )
  )
  def placeMarketOrder: Route = placeOrder("orderbook" / "market", isMarket = true)

  @Path("/orderbook")
  @ApiOperation(
    value = "Get the open trading markets",
    notes = "Get the open trading markets along with trading pairs meta data",
    httpMethod = "GET",
    response = classOf[ApiTradingMarkets]
  )
  def orderbooks: Route = (path("orderbook") & pathEndOrSingleSlash & get) {
    complete(
      (matcher ? GetMarkets).mapTo[Seq[MarketData]].map { markets =>
        SimpleResponse(
          ApiTradingMarkets(
            matcherPublicKey,
            markets.map { md =>
              val meta = orderBookInfo(md.pair)
              ApiMarketDataWithMeta(
                md.pair.amountAsset,
                md.amountAssetName,
                md.amountAssetInfo,
                md.pair.priceAsset,
                md.priceAssetName,
                md.priceAssetInfo,
                md.created,
                meta.restrictions,
                meta.matchingRules
              )
            }
          )
        )
      }
    )
  }

  private val handleBatchCancelResponse: LogicResponseHandler = {
    case AddressActor.Event.BatchCancelCompleted(xs) =>
      ApiSuccessfulBatchCancel(
        xs.map {
          case (id, Right(_)) => Right(ApiSuccessfulCancel(id))
          case (_, Left(e))   => Left(ApiError.from(e, "OrderCancelRejected"))
        }.toList
      )
    case x: error.MatcherError => StatusCodes.ServiceUnavailable -> ApiError.from(x, "BatchCancelRejected")
  }

  private def handleCancelRequest(assetPair: Option[AssetPair], sender: Address, orderId: Option[ByteStr], timestamp: Option[Long]): Route =
    complete {
      (timestamp, orderId) match {
        case (Some(ts), None) => askAddressActor(sender, AddressActor.Command.CancelAllOrders(assetPair, ts))(handleBatchCancelResponse)
        case (None, Some(oid)) =>
          askAddressActor(sender, AddressActor.Command.CancelOrder(oid)) {
            case AddressActor.Event.OrderCanceled(x) => SimpleResponse(ApiSuccessfulCancel(x))
            case x: error.MatcherError =>
              if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> ApiError.from(x, "WavesNodeUnavailable")
              else StatusCodes.BadRequest                                       -> ApiError.from(x, "OrderCancelRejected")
          }
        case _ => StatusCodes.BadRequest -> ApiError.from(error.CancelRequestIsIncomplete, "OrderCancelRejected")
      }
    }

  private def handleCancelRequest(assetPair: Option[AssetPair]): Route =
    withCancelRequest { req =>
      assetPair.fold(pass)(unavailableOrderBookBarrier).apply {
        handleCancelRequest(assetPair, req.sender, req.orderId, req.timestamp)
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/cancel")
  @ApiOperation(
    value = "Cancel order",
    notes = "Cancel previously submitted order if it's not already filled completely",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    response = classOf[ApiSuccessfulCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.CancelOrderRequest"
      )
    )
  )
  def cancel: Route = (path("orderbook" / AssetPairPM / "cancel") & post) { p =>
    withAssetPair(p, formatError = e => OrderCancelRejected(e)) { pair =>
      unavailableOrderBookBarrier(pair) {
        handleCancelRequest(Some(pair))
      }
    }
  }

  @Path("/orderbook/cancel")
  @ApiOperation(
    value = "Cancel all active orders",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    response = classOf[ApiSuccessfulBatchCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.CancelOrderRequest"
      )
    )
  )
  def cancelAll: Route = (path("orderbook" / "cancel") & post) {
    handleCancelRequest(None)
  }

  @Path("/orders/{address}/cancel")
  @ApiOperation(
    value = "Cancel active orders by IDs",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    produces = "application/json",
    consumes = "application/json",
    response = classOf[Any]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json array with order ids",
        required = true,
        paramType = "body",
        dataTypeClass = classOf[ApiSuccessfulBatchCancel]
      ),
    )
  )
  def cancelAllById: Route = (path("orders" / AddressPM / "cancel") & post & withAuth & withUserPublicKeyOpt) { (address, userPublicKey) =>
    userPublicKey match {
      case Some(upk) if upk.toAddress != address => invalidUserPublicKey
      case _ =>
        entity(as[Set[ByteStr]]) { xs =>
          complete { askAddressActor(address, AddressActor.Command.CancelOrders(xs))(handleBatchCancelResponse) }
        }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/delete")
  @Deprecated
  @ApiOperation(
    value = "Delete Order from History by Id",
    notes = "This method is deprecated and doesn't work anymore. Please don't use it.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    response = classOf[Any]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.CancelOrderRequest"
      )
    )
  )
  def historyDelete: Route = (path("orderbook" / AssetPairPM / "delete") & post) { _ =>
    post {
      entity(as[CancelOrderRequest]) { req =>
        complete {
          req.orderId.fold[MatcherResponse](NotImplemented(error.FeatureNotImplemented))(OrderDeleted)
        }
      }
    } ~ get(complete(StatusCodes.MethodNotAllowed))
  }

  private val tupledOrderBookHistoryItem: ((Id, OrderInfo[OrderStatus])) => ApiOrderBookHistoryItem =
    Function.tupled(ApiOrderBookHistoryItem.fromOrderInfo)

  private def loadOrders(address: Address, pair: Option[AssetPair], orderListType: OrderListType): Route = complete {
    askMapAddressActor[AddressActor.Reply.OrdersStatuses](address, AddressActor.Query.GetOrdersStatuses(pair, orderListType)) { reply =>
      StatusCodes.OK -> reply.xs.map(tupledOrderBookHistoryItem)
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/publicKey/{publicKey}")
  @ApiOperation(
    value = "Order History by Asset Pair and Public Key",
    notes = "Get Order History for a given Asset Pair and Public Key",
    httpMethod = "GET",
    response = classOf[Array[ApiOrderBookHistoryItem]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(
        name = "closedOnly",
        value = "Return closed only orders (Filled and Canceled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    )
  )
  def getAssetPairAndPublicKeyOrderHistory: Route = (path("orderbook" / AssetPairPM / "publicKey" / PublicKeyPM) & get) { (p, publicKey) =>
    withAssetPair(p, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
      parameters(('activeOnly.as[Boolean].?, 'closedOnly.as[Boolean].?)) { (activeOnly, closedOnly) =>
        signedGet(publicKey) {
          loadOrders(publicKey, Some(pair), getOrderListType(activeOnly, closedOnly, OrderListType.All))
        }
      }
    }
  }

  @Path("/orderbook/{publicKey}")
  @ApiOperation(
    value = "Order History by Public Key",
    notes = "Get Order History for a given Public Key",
    httpMethod = "GET",
    response = classOf[Array[ApiOrderBookHistoryItem]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(
        name = "closedOnly",
        value = "Return closed only orders (Filled and Canceled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    )
  )
  def getPublicKeyOrderHistory: Route = (path("orderbook" / PublicKeyPM) & get) { publicKey =>
    parameters(('activeOnly.as[Boolean].?, 'closedOnly.as[Boolean].?)) { (activeOnly, closedOnly) =>
      signedGet(publicKey) {
        loadOrders(publicKey, None, getOrderListType(activeOnly, closedOnly, OrderListType.All))
      }
    }
  }

  @Path("/orders/cancel/{orderId}")
  @ApiOperation(
    value = "Cancel Order by ID without signature",
    notes = "Cancel Order by ID without signature",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Any]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = `X-User-Public-Key`.headerName,
        value = "User's public key",
        required = false,
        dataType = "string",
        paramType = "header",
        defaultValue = ""
      ),
    )
  )
  def forceCancelOrder: Route = (path("orders" / "cancel" / ByteStrPM) & post & withAuth & withUserPublicKeyOpt) { (orderId, userPublicKey) =>
    def reject: StandardRoute = complete(OrderCancelRejected(error.OrderNotFound(orderId)))
    (DBUtils.order(db, orderId), userPublicKey) match {
      case (None, _)                                                         => reject
      case (Some(order), Some(pk)) if pk.toAddress != order.sender.toAddress => reject
      case (Some(order), _)                                                  => handleCancelRequest(None, order.sender, Some(orderId), None)
    }
  }

  @Path("/orders/{address}")
  @ApiOperation(
    value = "All Order History by address",
    notes = "Get All Order History for a given address",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Array[ApiOrderBookHistoryItem]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(
        name = "closedOnly",
        value = "Return closed only orders (Filled and Canceled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      )
    )
  )
  def getAllOrderHistory: Route = (path("orders" / AddressPM) & get & withAuth & withUserPublicKeyOpt) { (address, userPublicKey) =>
    userPublicKey match {
      case Some(upk) if upk.toAddress != address => invalidUserPublicKey
      case _ =>
        parameters(('activeOnly.as[Boolean].?, 'closedOnly.as[Boolean].?)) { (activeOnly, closedOnly) =>
          loadOrders(address, None, getOrderListType(activeOnly, closedOnly, OrderListType.ActiveOnly))
        }
    }
  }

  private def getOrderStatusInfo(id: Order.Id, address: Address): StandardRoute = complete {
    askMapAddressActor[AddressActor.Reply.OrdersStatusInfo](address, AddressActor.Query.GetOrderStatusInfo(id)) {
      _.maybeOrderStatusInfo match {
        case Some(oi) => SimpleResponse(ApiOrderBookHistoryItem.fromOrderInfo(id, oi))
        case None     => InfoNotFound(error.OrderNotFound(id))
      }
    }
  }

  @Path("/orders/{address}/{orderId}")
  @ApiOperation(
    value = "Order Status Info by address and ID without signature",
    notes = "Get Status Info of the specified order for a given address without signature",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[ApiOrderBookHistoryItem]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = `X-User-Public-Key`.headerName,
        value = "User's public key",
        required = false,
        dataType = "string",
        paramType = "header",
        defaultValue = ""
      )
    )
  )
  def getOrderStatusInfoByIdWithApiKey: Route = (path("orders" / AddressPM / ByteStrPM) & get & withAuth & withUserPublicKeyOpt) {
    (address, orderId, userPublicKey) =>
      userPublicKey match {
        case Some(upk) if upk.toAddress != address => invalidUserPublicKey
        case _                                     => getOrderStatusInfo(orderId, address)
      }
  }

  @Path("/orders/{publicKey}/{orderId}")
  @ApiOperation(
    value = "Order Status Info by public key and ID",
    notes = "Get Status Info of the specified order for a given public key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[ApiOrderBookHistoryItem]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    )
  )
  def getOrderStatusInfoByIdWithSignature: Route = (path("orders" / PublicKeyPM / ByteStrPM) & get) { (publicKey, orderId) =>
    signedGet(publicKey) { getOrderStatusInfo(orderId, publicKey.toAddress) }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}")
  @ApiOperation(
    value = "Tradable balance for Asset Pair",
    notes = "Get Tradable balance for the given Asset Pair",
    httpMethod = "GET",
    response = classOf[Map[String, Long]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "address", value = "Account Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def tradableBalance: Route = (path("orderbook" / AssetPairPM / "tradableBalance" / AddressPM) & get) { (pair, address) =>
    withAssetPair(pair, redirectToInverse = true, s"/tradableBalance/$address") { pair =>
      complete {
        askMapAddressActor[AddressActor.Reply.Balance](address, AddressActor.Query.GetTradableBalance(pair.assets)) { r =>
          ApiBalance(r.balance)
        }
      }
    }
  }

  @Path("/balance/reserved/{publicKey}")
  @ApiOperation(
    value = "Reserved Balance",
    notes = "Get non-zero balance of open orders",
    httpMethod = "GET",
    response = classOf[Map[String, Long]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    )
  )
  def reservedBalance: Route = (path("balance" / "reserved" / PublicKeyPM) & get) { publicKey =>
    (signedGet(publicKey).tmap(_ => Option.empty[PublicKey]) | (withAuth & withUserPublicKeyOpt)) {
      case Some(upk) if upk != publicKey => invalidUserPublicKey
      case _ =>
        complete {
          askMapAddressActor[AddressActor.Reply.Balance](publicKey, AddressActor.Query.GetReservedBalance) { r =>
            ApiBalance(r.balance)
          }
        }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/{orderId}")
  @ApiOperation(
    value = "Order Status",
    notes = "Get Order status for a given Asset Pair during the last 30 days",
    httpMethod = "GET",
    response = classOf[ApiOrderStatus]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
    )
  )
  def orderStatus: Route = (path("orderbook" / AssetPairPM / ByteStrPM) & get) { (p, orderId) =>
    withAssetPair(p, redirectToInverse = true, s"/$orderId") { _ =>
      complete {
        DBUtils.order(db, orderId) match {
          case Some(order) =>
            askMapAddressActor[AddressActor.Reply.GetOrderStatus](order.sender, AddressActor.Query.GetOrderStatus(orderId)) { r =>
              ApiOrderStatus.from(r.x)
            }

          case None =>
            Future.successful(DBUtils.orderInfo(db, orderId).fold(ApiOrderStatus.from(OrderStatus.NotFound))(x => ApiOrderStatus.from(x.status)))
        }
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(
    value = "Remove Order Book for a given Asset Pair",
    notes = "Remove Order Book for a given Asset Pair. Attention! Use this method only when clients can't place orders on this pair!",
    httpMethod = "DELETE",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Any]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def orderBookDelete: Route = (path("orderbook" / AssetPairPM) & delete & withAuth) { pair =>
    orderBook(pair) match {
      case Some(Right(_)) =>
        complete(
          storeEvent(QueueEvent.OrderBookDeleted(pair)).map {
            case None => NotImplemented(error.FeatureDisabled)
            case _    => SimpleResponse(ApiMessage("Deleting order book"), StatusCodes.Accepted)
          }
        )
      case _ => complete(OrderBookUnavailable(error.OrderBookBroken(pair)))
    }
  }

  @Path("/transactions/{orderId}")
  @ApiOperation(
    value = "Get Exchange Transactions for order",
    notes = "Get all exchange transactions created by DEX on execution of the given order",
    httpMethod = "GET",
    response = classOf[Array[ExchangeTransactionV2]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", dataType = "string", paramType = "path")
    )
  )
  def getTransactionsByOrder: Route = (path("transactions" / ByteStrPM) & get) { orderId =>
    complete { Json.toJson(DBUtils.transactionsForOrder(db, orderId)) }
  }

  @Path("/debug/currentOffset")
  @ApiOperation(
    value = "Get a current offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[QueueEventWithMeta.Offset]
  )
  def getCurrentOffset: Route = (path("debug" / "currentOffset") & get & withAuth) {
    complete { ApiOffset(currentOffset()) }
  }

  @Path("/debug/lastOffset")
  @ApiOperation(
    value = "Get the last offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[QueueEventWithMeta.Offset]
  )
  def getLastOffset: Route = (path("debug" / "lastOffset") & get & withAuth) {
    complete { lastOffset() map ApiOffset.apply }
  }

  @Path("/debug/oldestSnapshotOffset")
  @ApiOperation(
    value = "Get the oldest snapshot's offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[QueueEventWithMeta.Offset]
  )
  def getOldestSnapshotOffset: Route = (path("debug" / "oldestSnapshotOffset") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { response =>
        val defined = response.offsets.valuesIterator.collect { case Some(x) => x }
        val min     = if (defined.isEmpty) -1L else defined.min
        ApiOffset(min)
      }
    }
  }

  @Path("/debug/allSnapshotOffsets")
  @ApiOperation(
    value = "Get all snapshots' offsets in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[Map[String, QueueEventWithMeta.Offset]]
  )
  def getAllSnapshotOffsets: Route = (path("debug" / "allSnapshotOffsets") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { x =>
        ApiSnapshotOffsets(x.offsets.collect { case (assetPair, Some(offset)) => assetPair -> offset })
      }
    }
  }

  @Path("/debug/saveSnapshots")
  @ApiOperation(
    value = "Saves snapshots for all order books",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    response = classOf[String]
  )
  def saveSnapshots: Route = (path("debug" / "saveSnapshots") & post & withAuth) {
    complete {
      matcher ! ForceSaveSnapshots
      SimpleResponse(ApiMessage("Saving started"))
    }
  }

  @inline
  private def askMapAddressActor[A: ClassTag](sender: Address, msg: AddressActor.Message)(
      f: A => ToResponseMarshallable): Future[ToResponseMarshallable] = {
    (addressActor ? AddressDirectory.Envelope(sender, msg))
      .mapTo[A]
      .map(f)
      .recover {
        case e: AskTimeoutException =>
          log.error(s"Error processing $msg", e)
          TimedOut
      }
  }

  private val handleUnknownResponse: LogicResponseHandler = {
    case x =>
      log.error(s"Can't handle $x")
      api.InternalError
  }

  @inline
  private def askAddressActor(sender: Address, msg: AddressActor.Message)(handleResponse: LogicResponseHandler): Future[ToResponseMarshallable] = {
    (addressActor ? AddressDirectory.Envelope(sender, msg))
      .map(handleResponse.orElse(handleUnknownResponse))
      .recover {
        case e: AskTimeoutException =>
          log.error(s"Error processing $msg", e)
          TimedOut
      }
  }

  private def getOrderListType(activeOnly: Option[Boolean], closedOnly: Option[Boolean], default: OrderListType): OrderListType =
    if (activeOnly.isEmpty && closedOnly.isEmpty) default
    else
      (activeOnly.getOrElse(false), closedOnly.getOrElse(false)) match {
        case (true, true)   => OrderListType.Empty
        case (false, true)  => OrderListType.ClosedOnly
        case (true, false)  => OrderListType.ActiveOnly
        case (false, false) => OrderListType.All
      }
}
