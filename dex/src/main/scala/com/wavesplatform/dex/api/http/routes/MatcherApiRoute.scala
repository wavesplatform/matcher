package com.wavesplatform.dex.api.http.routes

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.FutureDirectives
import akka.pattern.{AskTimeoutException, ask}
import akka.stream.Materializer
import akka.util.Timeout
import cats.syntax.option._
import com.google.common.primitives.Longs
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex._
import com.wavesplatform.dex.actors.MatcherActor._
import com.wavesplatform.dex.actors.address.AddressActor.OrderListType
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.api.http._
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.headers.`X-User-Public-Key`
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.api.routes.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.transaction.ExchangeTransactionV2
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.{MatcherSettings, OrderFeeSettings}
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.Success

@Path("/matcher")
@Api()
class MatcherApiRoute(assetPairBuilder: AssetPairBuilder,
                      matcherPublicKey: PublicKey,
                      matcher: ActorRef,
                      addressActor: ActorRef,
                      storeEvent: StoreEvent,
                      orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                      orderBookHttpInfo: OrderBookHttpInfo,
                      getActualTickSize: AssetPair => BigDecimal,
                      orderValidator: Order => FutureResult[Order],
                      matcherSettings: MatcherSettings,
                      override val matcherStatus: () => Matcher.Status,
                      orderDb: OrderDB,
                      currentOffset: () => QueueEventWithMeta.Offset,
                      lastOffset: () => Future[QueueEventWithMeta.Offset],
                      matcherAccountFee: Long,
                      override val apiKeyHash: Option[Array[Byte]],
                      rateCache: RateCache,
                      validatedAllowedOrderVersions: () => Future[Set[Byte]],
                      getActualOrderFeeSettings: () => OrderFeeSettings)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with HasStatusBarrier
    with ScorexLogging {

  import com.wavesplatform.dex.api.routes.PathMatchers._

  private implicit val executionContext: ExecutionContext = mat.executionContext
  private implicit val timeout: Timeout                   = matcherSettings.actorResponseTimeout

  private type LogicResponseHandler = PartialFunction[Any, ToResponseMarshallable]

  private val timer      = Kamon.timer("matcher.api-requests")
  private val placeTimer = timer.withTag("action", "place")

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

  private def protect(unprotected: Route) = matcherStatusBarrier {
    handleExceptions(gRPCExceptionsHandler)(handleRejections(invalidJsonParsingRejectionsHandler)(unprotected))
  }

  private val ratesRoutes: Route        = pathPrefix("rates") { getRates ~ protect(upsertRate ~ deleteRate) }
  private val settingsRoutes: Route     = pathPrefix("settings") { getSettings ~ ratesRoutes }
  private val balanceRoutes: Route      = pathPrefix("balance") { protect(reservedBalance) }
  private val transactionsRoutes: Route = pathPrefix("transactions") { protect(getTransactionsByOrder) }

  private val debugRoutes: Route = pathPrefix("debug") {
    getCurrentOffset ~ getLastOffset ~ getOldestSnapshotOffset ~ getAllSnapshotOffsets ~ protect(saveSnapshots)
  }

  private val orderBookRoutes: Route = pathPrefix("orderbook") {
    protect {
      orderBookInfo ~ getOrderStatusInfoByIdWithSignature ~ getOrderBook ~ marketStatus ~ placeLimitOrder ~
        placeMarketOrder ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~ tradableBalance ~
        orderStatus ~ historyDelete ~ cancel ~ cancelAll ~ orderbooks ~ orderBookDelete
    }
  }

  private val ordersRoutes: Route = pathPrefix("orders") {
    protect { getAllOrderHistory ~ getOrderStatusInfoByIdWithApiKey ~ cancelAllById ~ forceCancelOrder }
  }

  override lazy val route: Route = pathPrefix("matcher") {
    getMatcherPublicKey ~ settingsRoutes ~ debugRoutes ~ orderBookRoutes ~ ordersRoutes ~ balanceRoutes ~ transactionsRoutes
  }

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

  private def withCorrectAddress(addressOrError: Either[ValidationError.InvalidAddress, Address])(f: Address => Route): Route =
    addressOrError.fold(ia => complete { InvalidAddress(ia.reason) }, f)

  private def withCancelRequest(f: HttpCancelOrder => Route): Route =
    post {
      entity(as[HttpCancelOrder]) { req =>
        if (req.isSignatureValid()) f(req) else complete(InvalidSignature)
      } ~ complete(StatusCodes.BadRequest)
    } ~ complete(StatusCodes.MethodNotAllowed)

  private def placeOrder(endpoint: Option[PathMatcher[Unit]], isMarket: Boolean): Route = {
    val route = (pathEndOrSingleSlash & entity(as[Order])) { order =>
      withAssetPair(order.assetPair, formatError = e => StatusCodes.BadRequest -> HttpError.from(e, "OrderRejected")) { pair =>
        unavailableOrderBookBarrier(pair) {
          complete(
            placeTimer.measureFuture {
              orderValidator(order).value flatMap {
                case Right(o) =>
                  placeTimer.measureFuture {
                    askAddressActor(o.sender, AddressActor.Command.PlaceOrder(o, isMarket)) {
                      case AddressActor.Event.OrderAccepted(x) => SimpleResponse(HttpSuccessfulPlace(x))
                      case x: error.MatcherError =>
                        if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> HttpError.from(x, "WavesNodeUnavailable")
                        else StatusCodes.BadRequest                                       -> HttpError.from(x, "OrderRejected")
                    }
                  }
                case Left(e) => Future.successful[ToResponseMarshallable] { StatusCodes.BadRequest -> HttpError.from(e, "OrderRejected") }
              }
            }
          )
        }
      }
    }

    endpoint.fold(route) { path(_)(route) }
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
  @ApiOperation(value = "Matcher Public Key",
                notes = "Get Matcher Public Key in Base58",
                httpMethod = "GET",
                tags = Array("info"),
                response = classOf[String])
  def getMatcherPublicKey: Route = (pathEndOrSingleSlash & get) { complete(matcherPublicKey.toJson) }

  @Path("/settings")
  @ApiOperation(value = "Matcher Settings",
                notes = "Get Matcher Public Settings",
                httpMethod = "GET",
                tags = Array("info"),
                response = classOf[HttpMatcherPublicSettings])
  def getSettings: Route = (pathEndOrSingleSlash & get) {
    complete(
      validatedAllowedOrderVersions() map { allowedOrderVersions =>
        SimpleResponse(
          HttpMatcherPublicSettings(
            matcherPublicKey = matcherPublicKey,
            matcherVersion = Version.VersionString,
            priceAssets = matcherSettings.priceAssets,
            orderFee = HttpOrderFeeMode.fromSettings(
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
    notes = "Get current rates of assets (price of 1 Waves in the specified asset), returns Map[Base58 encoded Asset ID, Double]",
    httpMethod = "GET",
    tags = Array("rates"),
    response = classOf[HttpRates]
  )
  def getRates: Route = (pathEndOrSingleSlash & get) { complete(rateCache.getAllRates.toJson) }

  @Path("/settings/rates/{assetId}")
  @ApiOperation(
    value = "Add or update rate for the specified asset",
    httpMethod = "PUT",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("rates"),
    response = classOf[HttpMessage]
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
    (path(AssetPM) & put & withAuth) { a =>
      entity(as[Double]) { rate =>
        if (rate <= 0) complete { RateError(error.NonPositiveAssetRate) } else
          withAsset(a) { asset =>
            complete(
              if (asset == Waves) RateError(error.WavesImmutableRate)
              else {
                val assetStr = asset.toString
                rateCache.upsertRate(asset, rate) match {
                  case None     => SimpleResponse(StatusCodes.Created, s"The rate $rate for the asset $assetStr added")
                  case Some(pv) => SimpleResponse(StatusCodes.OK, s"The rate for the asset $assetStr updated, old value = $pv, new value = $rate")
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
    tags = Array("rates"),
    response = classOf[HttpMessage]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is deleted", dataType = "string", paramType = "path")
    )
  )
  def deleteRate: Route = (path(AssetPM) & delete & withAuth) { a =>
    withAsset(a) { asset =>
      complete(
        if (asset == Waves) RateError(error.WavesImmutableRate)
        else {
          val assetStr = asset.toString
          rateCache.deleteRate(asset) match {
            case None     => RateError(error.RateNotFound(asset), StatusCodes.NotFound)
            case Some(pv) => SimpleResponse(StatusCodes.OK, s"The rate for the asset $assetStr deleted, old value = $pv")
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
    tags = Array("markets"),
    response = classOf[HttpV0OrderBook]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "depth",
                           value = "Limit the number of bid/ask records returned",
                           required = false,
                           dataType = "integer",
                           paramType = "query")
    )
  )
  def getOrderBook: Route = (path(AssetPairPM) & get) { p =>
    parameters("depth".as[Int].?) { depth =>
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
    tags = Array("markets"),
    response = classOf[HttpMarketStatus]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def marketStatus: Route = (path(AssetPairPM / "status") & get) { p =>
    withAssetPair(p, redirectToInverse = true, suffix = "/status") { pair =>
      complete { orderBookHttpInfo.getMarketStatus(pair) }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/info")
  @ApiOperation(value = "Get Order Restrictions for the specified Asset Pair",
                httpMethod = "GET",
                tags = Array("markets"),
                response = classOf[HttpOrderBookInfo])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def orderBookInfo: Route = (path(AssetPairPM / "info") & get) { p =>
    withAssetPair(p, redirectToInverse = true, suffix = "/info") { pair =>
      complete { SimpleResponse(orderBookInfo(pair)) }
    }
  }

  private def orderBookInfo(pair: AssetPair) = HttpOrderBookInfo(
    restrictions = matcherSettings.orderRestrictions.get(pair).map(HttpOrderRestrictions.fromSettings),
    matchingRules = HttpMatchingRules(tickSize = getActualTickSize(pair).toDouble)
  )

  @Path("/orderbook")
  @ApiOperation(
    value = "Place Limit Order",
    notes = "Place a new limit order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    tags = Array("place"),
    response = classOf[HttpSuccessfulPlace]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.domain.order.OrderV3"
      )
    )
  )
  def placeLimitOrder: Route = placeOrder(none, isMarket = false)

  @Path("/orderbook/market")
  @ApiOperation(
    value = "Place Market Order",
    notes = "Place a new market order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    tags = Array("place"),
    response = classOf[HttpSuccessfulPlace]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.domain.order.OrderV3"
      )
    )
  )
  def placeMarketOrder: Route = placeOrder(PathMatcher("market").some, isMarket = true)

  @Path("/orderbook")
  @ApiOperation(
    value = "Get the open trading markets",
    notes = "Get the open trading markets along with trading pairs meta data",
    httpMethod = "GET",
    tags = Array("markets"),
    response = classOf[HttpTradingMarkets]
  )
  def orderbooks: Route = (pathEndOrSingleSlash & get) {
    complete(
      (matcher ? GetMarkets).mapTo[Seq[MarketData]].map { markets =>
        SimpleResponse(
          HttpTradingMarkets(
            matcherPublicKey,
            markets.map { md =>
              val meta = orderBookInfo(md.pair)
              HttpMarketDataWithMeta(
                md.pair.amountAsset,
                md.amountAssetName,
                md.amountAssetInfo.map(HttpAssetInfo.fromAssetInfo),
                md.pair.priceAsset,
                md.priceAssetName,
                md.priceAssetInfo.map(HttpAssetInfo.fromAssetInfo),
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
      HttpSuccessfulBatchCancel(
        xs.map {
          case (id, Right(_)) => Right(HttpSuccessfulSingleCancel(id))
          case (_, Left(e))   => Left(HttpError.from(e, "OrderCancelRejected"))
        }.toList
      )
    case x: error.MatcherError => StatusCodes.ServiceUnavailable -> HttpError.from(x, "BatchCancelRejected")
  }

  private def handleCancelRequest(assetPair: Option[AssetPair], sender: Address, orderId: Option[ByteStr], timestamp: Option[Long]): Route =
    complete {
      (timestamp, orderId) match {
        case (Some(ts), None) =>
          askAddressActor(sender, AddressActor.Command.CancelAllOrders(assetPair, ts, AddressActor.Command.Source.Request))(handleBatchCancelResponse)
        case (None, Some(oid)) =>
          askAddressActor(sender, AddressActor.Command.CancelOrder(oid, AddressActor.Command.Source.Request)) {
            case AddressActor.Event.OrderCanceled(x) => SimpleResponse(HttpSuccessfulSingleCancel(x))
            case x: error.MatcherError =>
              if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> HttpError.from(x, "WavesNodeUnavailable")
              else StatusCodes.BadRequest                                       -> HttpError.from(x, "OrderCancelRejected")
          }
        case _ => StatusCodes.BadRequest -> HttpError.from(error.CancelRequestIsIncomplete, "OrderCancelRejected")
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
    value = "Cancel Order",
    notes = "Cancel previously submitted Order if it's not already filled completely",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    tags = Array("cancel"),
    response = classOf[HttpSuccessfulCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.http.protocol.HttpCancelOrder"
      )
    )
  )
  def cancel: Route = (path(AssetPairPM / "cancel") & post) { p =>
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
    tags = Array("cancel"),
    response = classOf[HttpSuccessfulBatchCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.http.protocol.HttpCancelOrder"
      )
    )
  )
  def cancelAll: Route = (path("cancel") & post) { handleCancelRequest(None) }

  @Path("/orders/{address}/cancel")
  @ApiOperation(
    value = "Cancel active orders by IDs",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    produces = "application/json",
    consumes = "application/json",
    tags = Array("cancel"),
    response = classOf[HttpSuccessfulBatchCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json array with Order IDs",
        required = true,
        paramType = "body",
        dataTypeClass = classOf[Array[String]]
      ),
    )
  )
  def cancelAllById: Route = (path(AddressPM / "cancel") & post & withAuth & withUserPublicKeyOpt) { (addressOrError, userPublicKey) =>
    withCorrectAddress(addressOrError) { address =>
      userPublicKey match {
        case Some(upk) if upk.toAddress != address => invalidUserPublicKey
        case _ =>
          entity(as[Set[ByteStr]]) { xs =>
            complete {
              askAddressActor(address, AddressActor.Command.CancelOrders(xs, AddressActor.Command.Source.Request))(handleBatchCancelResponse)
            }
          }
      }
    }
  }

  @Path("/orders/cancel/{orderId}")
  @ApiOperation(
    value = "Cancel Order by ID without signature",
    notes = "Cancel Order by ID without signature",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("cancel"),
    response = classOf[HttpSuccessfulSingleCancel]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order ID", required = true, dataType = "string", paramType = "path"),
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
  def forceCancelOrder: Route = (path("cancel" / ByteStrPM) & post & withAuth & withUserPublicKeyOpt) { (orderId, userPublicKey) =>
    def reject: StandardRoute = complete(OrderCancelRejected(error.OrderNotFound(orderId)))
    (orderDb.get(orderId), userPublicKey) match {
      case (None, _)                                                         => reject
      case (Some(order), Some(pk)) if pk.toAddress != order.sender.toAddress => reject
      case (Some(order), _)                                                  => handleCancelRequest(None, order.sender, Some(orderId), None)
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
    tags = Array("history"),
    response = classOf[HttpMessage]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.dex.api.http.protocol.HttpCancelOrder"
      )
    )
  )
  def historyDelete: Route = path(AssetPairPM / "delete") { _ =>
    post {
      entity(as[HttpCancelOrder]) { req =>
        complete {
          req.orderId.fold[MatcherResponse](NotImplemented(error.FeatureNotImplemented))(OrderDeleted)
        }
      }
    } ~ get(complete(StatusCodes.MethodNotAllowed))
  }

  private val tupledOrderBookHistoryItem: ((Id, OrderInfo[OrderStatus])) => HttpOrderBookHistoryItem =
    Function.tupled(HttpOrderBookHistoryItem.fromOrderInfo)

  private def loadOrders(address: Address, pair: Option[AssetPair], orderListType: OrderListType): Route = complete {
    askMapAddressActor[AddressActor.Reply.OrdersStatuses](address, AddressActor.Query.GetOrdersStatuses(pair, orderListType)) { reply =>
      reply.xs.map(tupledOrderBookHistoryItem)
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/publicKey/{publicKey}")
  @ApiOperation(
    value = "Order History by Asset Pair and Public Key",
    notes = "Get Order History for a given Asset Pair and Public Key",
    httpMethod = "GET",
    tags = Array("history"),
    response = classOf[Array[HttpOrderBookHistoryItem]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
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
      new ApiImplicitParam(
        name = "Signature",
        value = "Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(publicKey), bigEndianBytes(Timestamp)))",
        required = true,
        dataType = "string",
        paramType = "header"
      )
    )
  )
  def getAssetPairAndPublicKeyOrderHistory: Route = (path(AssetPairPM / "publicKey" / PublicKeyPM) & get) { (p, publicKey) =>
    withAssetPair(p, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
      parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
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
    tags = Array("history"),
    response = classOf[Array[HttpOrderBookHistoryItem]]
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
      new ApiImplicitParam(
        name = "Signature",
        value = "Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(publicKey), bigEndianBytes(Timestamp)))",
        required = true,
        dataType = "string",
        paramType = "header"
      )
    )
  )
  def getPublicKeyOrderHistory: Route = (path(PublicKeyPM) & get) { publicKey =>
    parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
      signedGet(publicKey) {
        loadOrders(publicKey, None, getOrderListType(activeOnly, closedOnly, OrderListType.All))
      }
    }
  }

  @Path("/orders/{address}")
  @ApiOperation(
    value = "All Order History by Address",
    notes = "Get All Order History for a given address",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("history"),
    response = classOf[Array[HttpOrderBookHistoryItem]]
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
  def getAllOrderHistory: Route = (path(AddressPM) & get & withAuth & withUserPublicKeyOpt) { (addressOrError, userPublicKey) =>
    withCorrectAddress(addressOrError) { address =>
      userPublicKey match {
        case Some(upk) if upk.toAddress != address => invalidUserPublicKey
        case _ =>
          parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
            loadOrders(address, None, getOrderListType(activeOnly, closedOnly, OrderListType.ActiveOnly))
          }
      }
    }
  }

  private def getOrderStatusInfo(id: Order.Id, address: Address): StandardRoute = complete {
    askMapAddressActor[AddressActor.Reply.OrdersStatusInfo](address, AddressActor.Query.GetOrderStatusInfo(id)) {
      _.maybeOrderStatusInfo match {
        case Some(oi) => SimpleResponse(HttpOrderBookHistoryItem.fromOrderInfo(id, oi))
        case None     => InfoNotFound(error.OrderNotFound(id))
      }
    }
  }

  @Path("/orders/{address}/{orderId}")
  @ApiOperation(
    value = "Order Status Info by Address and ID without signature",
    notes = "Get Status Info of the specified order for a given address without signature",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("status"),
    response = classOf[HttpOrderBookHistoryItem]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order ID", required = true, dataType = "string", paramType = "path"),
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
  def getOrderStatusInfoByIdWithApiKey: Route = (path(AddressPM / ByteStrPM) & get & withAuth & withUserPublicKeyOpt) {
    (addressOrError, orderId, userPublicKey) =>
      withCorrectAddress(addressOrError) { address =>
        userPublicKey match {
          case Some(upk) if upk.toAddress != address => invalidUserPublicKey
          case _                                     => getOrderStatusInfo(orderId, address)
        }
      }
  }

  // https://github.com/OAI/OpenAPI-Specification/issues/146#issuecomment-117288707
  @Path("/orderbook/{publicKey}/{orderId}#getOrderStatusInfoByIdWithSignature")
  @ApiOperation(
    value = "Order Status Info by Public Key and ID",
    notes = "Get Status Info of the specified order for a given public key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("status"),
    response = classOf[HttpOrderBookHistoryItem]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order ID", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(
        name = "Signature",
        value = "Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(publicKey), bigEndianBytes(Timestamp)))",
        required = true,
        dataType = "string",
        paramType = "header"
      )
    )
  )
  def getOrderStatusInfoByIdWithSignature: Route = (path(PublicKeyPM / ByteStrPM) & get) { (publicKey, orderId) =>
    signedGet(publicKey) { getOrderStatusInfo(orderId, publicKey.toAddress) }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}")
  @ApiOperation(
    value = "Tradable Balance for Asset Pair",
    notes = "Get Tradable Balance for the given Asset Pair, returns Map[Base58 encoded Asset ID, Long]",
    httpMethod = "GET",
    tags = Array("balances"),
    response = classOf[HttpBalance]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "address", value = "Account Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def tradableBalance: Route = (path(AssetPairPM / "tradableBalance" / AddressPM) & get) { (pair, addressOrError) =>
    withCorrectAddress(addressOrError) { address =>
      withAssetPair(pair, redirectToInverse = true, s"/tradableBalance/$address") { pair =>
        complete { askMapAddressActor[AddressActor.Reply.Balance](address, AddressActor.Query.GetTradableBalance(pair.assets)) { _.balance.toJson } }
      }
    }
  }

  @Path("/balance/reserved/{publicKey}")
  @ApiOperation(
    value = "Reserved Balance",
    notes = "Get non-zero balance of open orders, returns Map[Base58 encoded Asset ID, Long]",
    httpMethod = "GET",
    tags = Array("balances"),
    response = classOf[HttpBalance]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(
        name = "Signature",
        value = "Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(publicKey), bigEndianBytes(Timestamp)))",
        required = true,
        dataType = "string",
        paramType = "header"
      )
    )
  )
  def reservedBalance: Route = (path("reserved" / PublicKeyPM) & get) { publicKey =>
    (signedGet(publicKey).tmap(_ => Option.empty[PublicKey]) | (withAuth & withUserPublicKeyOpt)) {
      case Some(upk) if upk != publicKey => invalidUserPublicKey
      case _ =>
        complete {
          askMapAddressActor[AddressActor.Reply.Balance](publicKey, AddressActor.Query.GetReservedBalance) { _.balance.toJson }
        }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/{orderId}")
  @ApiOperation(
    value = "Order Status",
    notes = "Get Order status for a given Asset Pair during the last 30 days",
    httpMethod = "GET",
    tags = Array("status"),
    response = classOf[HttpOrderStatus]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order ID", required = true, dataType = "string", paramType = "path")
    )
  )
  def orderStatus: Route = (path(AssetPairPM / ByteStrPM) & get) { (p, orderId) =>
    withAssetPair(p, redirectToInverse = true, s"/$orderId") { _ =>
      complete {
        orderDb.get(orderId) match {
          case Some(order) =>
            askMapAddressActor[AddressActor.Reply.GetOrderStatus](order.sender, AddressActor.Query.GetOrderStatus(orderId)) { r =>
              HttpOrderStatus.from(r.x)
            }
          case None =>
            Future.successful(orderDb.getOrderInfo(orderId).fold(HttpOrderStatus.from(OrderStatus.NotFound))(x => HttpOrderStatus.from(x.status)))
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
    tags = Array("markets"),
    response = classOf[HttpMessage]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def orderBookDelete: Route = (path(AssetPairPM) & delete & withAuth) { pair =>
    orderBook(pair) match {
      case Some(Right(_)) =>
        complete(
          storeEvent(QueueEvent.OrderBookDeleted(pair))
            .map {
              case None => NotImplemented(error.FeatureDisabled)
              case _    => SimpleResponse(StatusCodes.Accepted, "Deleting order book")
            }
            .recover {
              case e: Throwable =>
                log.error("Can not persist event", e)
                CanNotPersist(error.CanNotPersistEvent)
            }
        )
      case _ => complete(OrderBookUnavailable(error.OrderBookBroken(pair)))
    }
  }

  @Path("/transactions/{orderId}")
  @ApiOperation(
    value = "Get Exchange Transactions by Order",
    notes = "Get all exchange transactions created by DEX on execution of the given order",
    httpMethod = "GET",
    tags = Array("transactions"),
    response = classOf[Array[ExchangeTransactionV2]]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order ID", dataType = "string", paramType = "path")
    )
  )
  def getTransactionsByOrder: Route = (path(ByteStrPM) & get) { orderId =>
    complete { Json.toJson(orderDb.transactionsByOrder(orderId)) }
  }

  @Path("/debug/currentOffset")
  @ApiOperation(
    value = "Get the current offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getCurrentOffset: Route = (path("currentOffset") & get & withAuth) { complete(currentOffset().toJson) }

  @Path("/debug/lastOffset")
  @ApiOperation(
    value = "Get the last offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getLastOffset: Route = (path("lastOffset") & get & withAuth) {
    complete { lastOffset() map (_.toJson) }
  }

  @Path("/debug/oldestSnapshotOffset")
  @ApiOperation(
    value = "Get the oldest snapshot's offset in the queue",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getOldestSnapshotOffset: Route = (path("oldestSnapshotOffset") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { response =>
        val defined = response.offsets.valuesIterator.collect { case Some(x) => x }
        (if (defined.isEmpty) -1L else defined.min).toJson
      }
    }
  }

  @Path("/debug/allSnapshotOffsets")
  @ApiOperation(
    value = "Get all snapshots' offsets in the queue",
    notes = "Returns Map[Asset Pair, Long]",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpSnapshotOffsets]
  )
  def getAllSnapshotOffsets: Route = (path("allSnapshotOffsets") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { x =>
        x.offsets.collect { case (assetPair, Some(offset)) => assetPair -> offset }.toJson
      }
    }
  }

  @Path("/debug/saveSnapshots")
  @ApiOperation(
    value = "Saves snapshots for all Order Books",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpMessage]
  )
  def saveSnapshots: Route = (path("saveSnapshots") & post & withAuth) {
    complete {
      matcher ! ForceSaveSnapshots
      SimpleResponse(StatusCodes.OK, "Saving started")
    }
  }

  private def askMapAddressActor[A: ClassTag](sender: Address, msg: AddressActor.Message)(
      f: A => ToResponseMarshallable): Future[ToResponseMarshallable] = {
    (addressActor ? AddressDirectoryActor.Envelope(sender, msg))
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
      entities.InternalError
  }

  @inline
  private def askAddressActor(sender: Address, msg: AddressActor.Message)(handleResponse: LogicResponseHandler): Future[ToResponseMarshallable] = {
    (addressActor ? AddressDirectoryActor.Envelope(sender, msg))
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
