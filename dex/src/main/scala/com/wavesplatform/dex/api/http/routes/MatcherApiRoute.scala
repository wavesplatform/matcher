package com.wavesplatform.dex.api.http.routes

import akka.actor.{typed, ActorRef}
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.FutureDirectives
import akka.pattern.{ask, AskTimeoutException}
import akka.stream.Materializer
import akka.util.Timeout
import cats.instances.future._
import cats.instances.list._
import cats.syntax.option._
import cats.syntax.traverse._
import com.google.common.primitives.Longs
import com.typesafe.config.Config
import com.wavesplatform.dex._
import com.wavesplatform.dex.actors.OrderBookDirectoryActor._
import com.wavesplatform.dex.actors.address.AddressActor.OrderListType
import com.wavesplatform.dex.actors.address.AddressActor.Query.GetCurrentState
import com.wavesplatform.dex.actors.address.AddressActor.Reply.GetState
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.api.http._
import com.wavesplatform.dex.api.http.directives.HttpKamonMetricsDirectives._
import com.wavesplatform.dex.api.http.directives.ProtectDirective
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.headers.{`X-User-Public-Key`, CustomContentTypes}
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.api.routes.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.api.ws.actors.WsExternalClientDirectoryActor
import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.OrderDb
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
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.MatcherQueue.StoreValidatedCommand
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import com.wavesplatform.dex.settings.{MatcherSettings, OrderFeeSettings}
import io.swagger.annotations._
import kamon.Kamon
import play.api.libs.json._

import javax.ws.rs.Path
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.Success

/**
 * @param getActualTickSize We need FutureResult, because it is used internally in methods which face with a potentially unknown assets
 */
@Path("/matcher")
@Api()
class MatcherApiRoute(
  assetPairBuilder: AssetPairBuilder,
  matcherPublicKey: PublicKey,
  safeConfig: Config,
  matcher: ActorRef,
  addressActor: ActorRef,
  blockchainStatus: => CombinedStream.Status,
  storeCommand: StoreValidatedCommand,
  orderBook: AssetPair => Option[Either[Unit, ActorRef]],
  orderBookHttpInfo: OrderBookHttpInfo,
  getActualTickSize: AssetPair => FutureResult[BigDecimal],
  orderValidator: Order => FutureResult[Order],
  matcherSettings: MatcherSettings,
  override val matcherStatus: () => MatcherStatus,
  orderDb: OrderDb[Future],
  currentOffset: () => ValidatedCommandWithMeta.Offset,
  lastOffset: () => Future[ValidatedCommandWithMeta.Offset],
  matcherAccountFee: Long,
  override val apiKeyHash: Option[Array[Byte]],
  rateCache: RateCache,
  validatedAllowedOrderVersions: () => Future[Set[Byte]],
  getActualOrderFeeSettings: () => OrderFeeSettings,
  externalClientDirectoryRef: typed.ActorRef[WsExternalClientDirectoryActor.Message],
  getAssetDescription: Asset => FutureResult[BriefAssetDescription]
)(implicit mat: Materializer)
    extends ApiRoute
    with ProtectDirective
    with HasStatusBarrier
    with AuthRoute
    with ScorexLogging {

  import com.wavesplatform.dex.api.routes.PathMatchers._

  implicit private val executionContext: ExecutionContext = mat.executionContext
  implicit private val timeout: Timeout = matcherSettings.actorResponseTimeout

  private type LogicResponseHandler = PartialFunction[Any, ToResponseMarshallable]

  private val timer = Kamon.timer("matcher.api-requests")
  private val placeTimer = timer.withTag("action", "place")

  private val invalidUserPublicKey: StandardRoute = complete(SimpleErrorResponse(StatusCodes.Forbidden, error.UserPublicKeyIsNotValid()))

  private val ratesRoutes: Route = pathPrefix("rates") {
    getAssetRates ~ matcherStatusBarrier(upsertAssetRate ~ deleteAssetRate)
  }

  private val settingsRoutes: Route = pathPrefix("settings")(getMatcherPublicSettings ~ ratesRoutes)
  private val balanceRoutes: Route = pathPrefix("balance")(getReservedBalanceByPK)
  private val transactionsRoutes: Route = pathPrefix("transactions")(getTransactionsByOrderId)

  private val debugRoutes: Route = pathPrefix("debug") {
    getMatcherStatus ~ getAddressState ~ getMatcherConfig ~ getCurrentOffset ~ getLastOffset ~
    getOldestSnapshotOffset ~ getAllSnapshotOffsets ~ saveSnapshots ~ printMessage
  }

  private val orderBookRoutes: Route = pathPrefix("orderbook") {
    matcherStatusBarrier {
      getOrderBookRestrictions ~ getOrderStatusByPKAndIdWithSig ~ getOrderBook ~ getOrderBookStatus ~ placeLimitOrder ~ placeMarketOrder ~
      getOrderHistoryByAssetPairAndPKWithSig ~ getOrderHistoryByPKWithSig ~ getTradableBalanceByAssetPairAndAddress ~ orderStatusByAssetPairAndId ~ deleteOrderFromHistoryById ~ cancelOneOrAllInPairOrdersWithSig ~
      cancelAllOrdersWithSig ~ getOrderBooks ~ deleteOrderBookWithKey
    }
  }

  private val ordersRoutes: Route = pathPrefix("orders") {
    matcherStatusBarrier(getOrderHistoryByAddressWithKey ~ getOrderStatusByAddressAndIdWithKey ~ cancelOrdersByIdsWithKey ~ cancelOneOrderWithKey)
  }

  override lazy val route: Route = pathPrefix("matcher") {
    getMatcherPKInBase58 ~ settingsRoutes ~ debugRoutes ~ orderBookRoutes ~ ordersRoutes ~ balanceRoutes ~ transactionsRoutes
  }

  private def withAssetPair(
    pairOrError: Either[ValidationError.InvalidAsset, AssetPair],
    redirectToInverse: Boolean = false,
    suffix: String = "",
    formatError: MatcherError => ToResponseMarshallable = InfoNotFound.apply,
    validate: Boolean = true
  ): Directive1[AssetPair] =
    pairOrError match {
      case Right(p) =>
        if (validate)
          FutureDirectives.onSuccess(assetPairBuilder.validateAssetPair(p).value) flatMap {
            case Right(_) => provide(p)
            case Left(e) if redirectToInverse =>
              FutureDirectives.onSuccess(assetPairBuilder.validateAssetPair(p.reverse).value) flatMap {
                case Right(_) => redirect(s"/matcher/orderbook/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
                case Left(_) => complete(formatError(e))
              }
            case Left(e) => complete(formatError(e))
          }
        else provide(p)
      case Left(ia) => complete(InvalidAsset(ia.asset, ia.reason))
    }

  private def withAsset(assetOrError: Either[ValidationError.InvalidAsset, Asset]): Directive1[Asset] =
    assetOrError match {
      case Right(a) => FutureDirectives.onSuccess(assetPairBuilder.validateAssetId(a).value) flatMap {
          case Right(_) => provide(a)
          case Left(e) => complete(InfoNotFound(e))
        }
      case Left(ia) => complete(InvalidAsset(ia.asset, ia.reason))
    }

  private def withOrderId(orderIdOrError: Either[ValidationError.InvalidBase58String, ByteStr])(f: ByteStr => Route): Route =
    orderIdOrError.fold(io => complete(InvalidBase58String(io.reason)), f)

  private def withPublicKey(publicKeyOrError: Either[ValidationError.InvalidPublicKey, PublicKey])(f: PublicKey => Route): Route =
    publicKeyOrError.fold(ipk => complete(InvalidPublicKey(ipk.reason)), f)

  private def withAddress(addressOrError: Either[ValidationError.InvalidAddress, Address])(f: Address => Route): Route =
    addressOrError.fold(ia => complete(InvalidAddress(ia.reason)), f)

  private def withCancelRequest(f: HttpCancelOrder => Route): Route =
    post {
      entity(as[HttpCancelOrder]) { req =>
        if (req.isSignatureValid()) f(req) else complete(InvalidSignature)
      } ~ complete(StatusCodes.BadRequest)
    } ~ complete(StatusCodes.MethodNotAllowed)

  // DEX-1192 docs/places-and-cancels.md
  private def placeOrder(endpoint: Option[PathMatcher[Unit]], isMarket: Boolean): Route = {
    val orderType = if (isMarket) "Market" else "Limit"
    val route = (pathEndOrSingleSlash & post & measureResponse(s"place${orderType}Order") & protect & entity(as[Order])) { order =>
      withAssetPair(Right(order.assetPair), formatError = e => StatusCodes.BadRequest -> HttpError.from(e, "OrderRejected")) { pair =>
        complete(
          placeTimer.measureFuture {
            orderValidator(order).value flatMap {
              case Right(o) =>
                placeTimer.measureFuture {
                  askAddressActor(o.sender, AddressActor.Command.PlaceOrder(o, isMarket)) {
                    case AddressActor.Event.OrderAccepted(x) => SimpleResponse(HttpSuccessfulPlace(x))
                    case x: error.MatcherError =>
                      if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> HttpError.from(x, "WavesNodeUnavailable")
                      else StatusCodes.BadRequest -> HttpError.from(x, "OrderRejected")
                  }
                }
              case Left(e) => Future.successful[ToResponseMarshallable](StatusCodes.BadRequest -> HttpError.from(e, "OrderRejected"))
            }
          }
        )
      }
    }

    endpoint.fold(route)(path(_)(route))
  }

  private def signedGet(publicKey: PublicKey): Directive0 =
    (headerValueByName("Timestamp") & headerValueByName("Signature")).tflatMap { case (timestamp, sig) =>
      Base58.tryDecodeWithLimit(sig).map(crypto.verify(_, publicKey ++ Longs.toByteArray(timestamp.toLong), publicKey)) match {
        case Success(true) => pass
        case _ => complete(InvalidSignature)
      }
    }

  @Path("/#getMatcherPKInBase58")
  @ApiOperation(
    value = "Matcher Public Key",
    notes = "Get Matcher Public Key in Base58",
    httpMethod = "GET",
    tags = Array("info"),
    response = classOf[String]
  )
  def getMatcherPKInBase58: Route =
    (pathEndOrSingleSlash & get)((measureResponse("getMatcherPKInBase58") & protect)(complete(matcherPublicKey.toJson)))

  @Path("/settings#getMatcherPublicSettings")
  @ApiOperation(
    value = "Matcher Settings",
    notes = "Get Matcher Public Settings",
    httpMethod = "GET",
    tags = Array("info"),
    response = classOf[HttpMatcherPublicSettings]
  )
  def getMatcherPublicSettings: Route =
    (pathEndOrSingleSlash & get) {
      measureResponse("getMatcherPublicSettings") {
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
    }

  @Path("/settings/rates#getAssetRates")
  @ApiOperation(
    value = "Asset rates",
    notes = "Get current rates of assets (price of 1 Waves in the specified asset), returns Map[Base58 encoded Asset ID, Double]",
    httpMethod = "GET",
    tags = Array("rates"),
    response = classOf[HttpRates]
  )
  def getAssetRates: Route = (pathEndOrSingleSlash & get)(measureResponse("getAssetRates")(complete(rateCache.getAllRates.toJson)))

  @Path("/settings/rates/{assetId}#upsertAssetRate")
  @ApiOperation(
    value = "Add or update rate for the specified asset. Requires API Key",
    httpMethod = "PUT",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("rates"),
    response = classOf[HttpMessage]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is added or updated", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "rate",
        value = "Rate associated with the specified asset",
        dataType = "double",
        paramType = "body",
        required = true
      )
    )
  )
  def upsertAssetRate: Route =
    (path(AssetPM) & put) { assetOrError =>
      (measureResponse("upsertAssetRate") & protect & withAuth) {
        entity(as[Double]) { rate =>
          if (rate.isInfinite || rate <= 0)
            complete(RateError(error.InvalidAssetRate))
          else
            withAsset(assetOrError) { asset =>
              complete(
                if (asset == Waves) RateError(error.WavesImmutableRate)
                else {
                  val assetStr = asset.toString
                  val response = rateCache.upsertRate(asset, rate) match {
                    case None => SimpleResponse(StatusCodes.Created, s"The rate $rate for the asset $assetStr added")
                    case Some(pv) =>
                      SimpleResponse(StatusCodes.OK, s"The rate for the asset $assetStr updated, old value = $pv, new value = $rate")
                  }
                  externalClientDirectoryRef ! WsExternalClientDirectoryActor.Command.BroadcastRatesUpdates(Map(asset -> rate))
                  response
                }
              )
            }
        }
      }
    }

  @Path("/settings/rates/{assetId}#deleteAssetRate")
  @ApiOperation(
    value = "Delete rate for the specified asset. Requires API Key",
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
  def deleteAssetRate: Route =
    (path(AssetPM) & delete) { assetOrError =>
      (measureResponse("deleteAssetRate") & protect & withAuth) {
        withAsset(assetOrError) { asset =>
          complete(
            if (asset == Waves) RateError(error.WavesImmutableRate)
            else {
              val assetStr = asset.toString
              val response = rateCache.deleteRate(asset) match {
                case None => RateError(error.RateNotFound(asset), StatusCodes.NotFound)
                case Some(pv) => SimpleResponse(StatusCodes.OK, s"The rate for the asset $assetStr deleted, old value = $pv")
              }
              externalClientDirectoryRef ! WsExternalClientDirectoryActor.Command.BroadcastRatesUpdates(Map(asset -> -1))
              response
            }
          )
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}#getOrderBook")
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
      new ApiImplicitParam(
        name = "depth",
        value = "Limit the number of bid/ask records returned",
        required = false,
        dataType = "integer",
        paramType = "query"
      )
    )
  )
  def getOrderBook: Route =
    (path(AssetPairPM) & get) { pairOrError =>
      (measureResponse("getOrderBook") & protect) {
        parameters("depth".as[String].?) {
          case None => withAssetPair(pairOrError, redirectToInverse = true, "") { pair =>
              complete(orderBookHttpInfo.getHttpView(pair, MatcherModel.Normalized, None))
            }
          case Some(depth) =>
            depth.toIntOption match {
              case None => complete(InvalidDepth(s"Depth value '$depth' must be an Integer"))
              case Some(d) =>
                if (d >= 0) withAssetPair(pairOrError, redirectToInverse = true, s"?depth=$d") { pair =>
                  complete(orderBookHttpInfo.getHttpView(pair, MatcherModel.Normalized, Some(d)))
                }
                else complete(InvalidDepth(s"Depth value '$depth' must be non-negative"))
            }
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/status#getOrderBookStatus")
  @ApiOperation(
    value = "Get Market Status",
    notes = "Get current market data such as last trade, best bid and ask",
    httpMethod = "GET",
    tags = Array("markets"),
    response = classOf[HttpOrderBookStatus]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def getOrderBookStatus: Route =
    (path(AssetPairPM / "status") & get) { pairOrError =>
      (measureResponse("getOrderBookStatus") & protect) {
        withAssetPair(pairOrError, redirectToInverse = true, suffix = "/status") { pair =>
          complete(orderBookHttpInfo.getMarketStatus(pair))
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/info#getOrderBookRestrictions")
  @ApiOperation(
    value = "Get Order Restrictions for the specified Asset Pair",
    httpMethod = "GET",
    tags = Array("markets"),
    response = classOf[HttpOrderBookInfo]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset ID in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    )
  )
  def getOrderBookRestrictions: Route =
    (path(AssetPairPM / "info") & get) { pairOrError =>
      (measureResponse("getOrderBookRestrictions") & protect) {
        withAssetPair(pairOrError, redirectToInverse = true, suffix = "/info") { pair =>
          complete(getOrderBookRestrictions(pair).value.map {
            case Right(x) => SimpleResponse(x)
            case Left(e) => InfoNotFound(e)
          })
        }
      }
    }

  private def getOrderBookRestrictions(pair: AssetPair): FutureResult[HttpOrderBookInfo] = getActualTickSize(pair).map { tickSize =>
    HttpOrderBookInfo(
      restrictions = matcherSettings.orderRestrictions.get(pair).map(HttpOrderRestrictions.fromSettings),
      matchingRules = HttpMatchingRules(tickSize = tickSize.toDouble)
    )
  }

  @Path("/orderbook#placeLimitOrder")
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

  @Path("/orderbook/market#placeMarketOrder")
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

  @Path("/orderbook#getOrderBooks")
  @ApiOperation(
    value = "Get the open trading markets",
    notes = "Get the open trading markets along with trading pairs meta data",
    httpMethod = "GET",
    tags = Array("markets"),
    response = classOf[HttpTradingMarkets]
  )
  def getOrderBooks: Route =
    (pathEndOrSingleSlash & get) {
      (measureResponse("getOrderBooks") & protect) {
        complete(
          (matcher ? GetMarkets).mapTo[List[MarketData]].flatMap { markets =>
            markets
              .map { md =>
                getOrderBookRestrictions(md.pair)
                  .map { meta =>
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
                  .value
              }
              .sequence
              .map(_.collect { case Right(x) => x })
              .map(x => SimpleResponse(HttpTradingMarkets(matcherPublicKey, x)))
          }
        )
      }
    }

  private val handleBatchCancelResponse: LogicResponseHandler = {
    case AddressActor.Event.BatchCancelCompleted(xs) =>
      HttpSuccessfulBatchCancel(
        xs.map {
          case (id, Right(_)) => Right(HttpSuccessfulSingleCancel(id))
          case (_, Left(e)) => Left(HttpError.from(e, "OrderCancelRejected"))
        }.toList
      )
    case x: error.MatcherError => StatusCodes.ServiceUnavailable -> HttpError.from(x, "BatchCancelRejected")
  }

  private def handleCancelRequestToFuture(
    assetPair: Option[AssetPair],
    sender: Address,
    orderId: Option[ByteStr],
    timestamp: Option[Long]
  ): Future[ToResponseMarshallable] =
    (timestamp, orderId) match {
      case (Some(ts), None) =>
        askAddressActor(sender, AddressActor.Command.CancelAllOrders(assetPair, ts, AddressActor.Command.Source.Request))(
          handleBatchCancelResponse
        )
      case (None, Some(oid)) =>
        askAddressActor(sender, AddressActor.Command.CancelOrder(oid, AddressActor.Command.Source.Request)) {
          case AddressActor.Event.OrderCanceled(x) => SimpleResponse(HttpSuccessfulSingleCancel(x))
          case x: error.MatcherError =>
            if (x == error.CanNotPersistEvent) StatusCodes.ServiceUnavailable -> HttpError.from(x, "WavesNodeUnavailable")
            else StatusCodes.BadRequest -> HttpError.from(x, "OrderCancelRejected")
        }
      case _ =>
        Future.successful(StatusCodes.BadRequest -> HttpError.from(error.CancelRequestIsIncomplete, "OrderCancelRejected"))
    }

  private def handleCancelRequestToRoute(assetPair: Option[AssetPair], sender: Address, orderId: Option[ByteStr], timestamp: Option[Long]): Route =
    complete(handleCancelRequestToFuture(assetPair, sender, orderId, timestamp): Future[ToResponseMarshallable])

  private def handleCancelRequestToRoute(assetPair: Option[AssetPair]): Route =
    withCancelRequest { req =>
      handleCancelRequestToRoute(assetPair, req.sender, req.orderId, req.timestamp)
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/cancel#cancelOneOrAllInPairOrdersWithSig")
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
  def cancelOneOrAllInPairOrdersWithSig: Route = // DEX-1192 docs/places-and-cancels.md
    (path(AssetPairPM / "cancel") & post) { pairOrError =>
      (measureResponse("cancelOneOrAllInPairOrdersWithSig") & protect) {
        withAssetPair(pairOrError, formatError = e => OrderCancelRejected(e)) { pair =>
          handleCancelRequestToRoute(Some(pair))

        }
      }
    }

  @Path("/orderbook/cancel#cancelAllOrdersWithSig")
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
  def cancelAllOrdersWithSig: Route =
    (path("cancel") & post)((measureResponse("cancelAllOrdersWithSig") & protect)(handleCancelRequestToRoute(None)))

  @Path("/orders/{address}/cancel#cancelOrdersByIdsWithKey")
  @ApiOperation(
    value = "Cancel active orders by IDs. Requires API Key",
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
      )
    )
  )
  def cancelOrdersByIdsWithKey: Route =
    (path(AddressPM / "cancel") & post) { addressOrError =>
      (measureResponse("cancelAllByApiKeyAndIds") & protect) {
        (withAuth & withUserPublicKeyOpt) { userPublicKey =>
          withAddress(addressOrError) { address =>
            userPublicKey match {
              case Some(upk) if upk.toAddress != address => invalidUserPublicKey
              case _ =>
                entity(as[Set[ByteStr]]) { xs =>
                  complete {
                    askAddressActor(address, AddressActor.Command.CancelOrders(xs, AddressActor.Command.Source.Request))(
                      handleBatchCancelResponse
                    )
                  }
                }
            }
          }
        }
      }
    }

  @Path("/orders/cancel/{orderId}#cancelOneOrderWithKey")
  @ApiOperation(
    value = "Cancel Order by ID without signature. Requires API Key",
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
      )
    )
  )
  def cancelOneOrderWithKey: Route =
    (path("cancel" / OrderPM) & post) { orderIdOrError =>
      (measureResponse("cancelOneOrderWithKey") & protect) {
        (withAuth & withUserPublicKeyOpt) { maybeUserPublicKey =>
          withOrderId(orderIdOrError) { orderId =>
            complete {
              maybeUserPublicKey match {
                case Some(userPublicKey) =>
                  handleCancelRequestToFuture(None, userPublicKey.toAddress, Some(orderId), None)
                case None =>
                  orderDb.get(orderId).flatMap {
                    case Some(order) => handleCancelRequestToFuture(None, order.sender, Some(orderId), None)
                    case None => Future.successful(ToResponseMarshallable(OrderCancelRejected(error.OrderNotFound(orderId))))
                  }.recover { case th =>
                    log.error("error while cancelling order", th)
                    ToResponseMarshallable(entities.InternalError)
                  }
              }
            }
          }
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/delete#deleteOrderFromHistoryById")
  @Deprecated
  @ApiOperation(
    value = "Delete Order from History by Id",
    notes = "This method is deprecated and doesn't work anymore. Please don't use it.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    tags = Array("history"),
    response = classOf[HttpSuccessfulDelete]
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
  def deleteOrderFromHistoryById: Route =
    path(AssetPairPM / "delete") { _ =>
      (measureResponse("deleteOrderFromHistoryById") & protect) {
        post {
          entity(as[HttpCancelOrder]) { req =>
            complete {
              req.orderId.fold[MatcherResponse](NotImplemented(error.FeatureNotImplemented))(OrderDeleted)
            }
          }
        } ~ get(complete(StatusCodes.MethodNotAllowed))
      }
    }

  private val tupledOrderBookHistoryItem: ((Id, OrderInfo[OrderStatus])) => HttpOrderBookHistoryItem =
    Function.tupled(HttpOrderBookHistoryItem.fromOrderInfo)

  private def loadOrders(address: Address, pair: Option[AssetPair], orderListType: OrderListType): Route = complete {
    askMapAddressActor[AddressActor.Reply.GetOrderStatuses](address, AddressActor.Query.GetOrdersStatuses(pair, orderListType)) { reply =>
      reply.xs.map(tupledOrderBookHistoryItem)
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/publicKey/{publicKey}#getOrderHistoryByAssetPairAndPKWithSig")
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
  def getOrderHistoryByAssetPairAndPKWithSig: Route =
    (path(AssetPairPM / "publicKey" / PublicKeyPM) & get) { (pairOrError, publicKeyOrError) =>
      (measureResponse("getOrderHistoryByAssetPairAndPKWithSig") & protect) {
        withPublicKey(publicKeyOrError) { publicKey =>
          withAssetPair(pairOrError, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
            parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
              signedGet(publicKey) {
                loadOrders(publicKey, Some(pair), getOrderListType(activeOnly, closedOnly, OrderListType.All))
              }
            }
          }
        }
      }
    }

  @Path("/orderbook/{publicKey}#getOrderHistoryByPKWithSig")
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
  def getOrderHistoryByPKWithSig: Route =
    (path(PublicKeyPM) & get) { publicKeyOrError =>
      (measureResponse("getOrderHistoryByPKWithSig") & protect) {
        parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
          withPublicKey(publicKeyOrError) { publicKey =>
            signedGet(publicKey) {
              loadOrders(publicKey, None, getOrderListType(activeOnly, closedOnly, OrderListType.All))
            }
          }
        }
      }
    }

  @Path("/orders/{address}#getOrderHistoryByAddressWithKey")
  @ApiOperation(
    value = "All Order History by Address. Requires API Key",
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
  def getOrderHistoryByAddressWithKey: Route =
    (path(AddressPM) & get) { addressOrError =>
      (measureResponse("getOrderHistoryByApiKey") & protect) {
        (withAuth & withUserPublicKeyOpt) { userPublicKey =>
          withAddress(addressOrError) { address =>
            userPublicKey match {
              case Some(upk) if upk.toAddress != address => invalidUserPublicKey
              case _ =>
                parameters("activeOnly".as[Boolean].?, "closedOnly".as[Boolean].?) { (activeOnly, closedOnly) =>
                  loadOrders(address, None, getOrderListType(activeOnly, closedOnly, OrderListType.ActiveOnly))
                }
            }
          }
        }
      }
    }

  private def getOrderStatusInfo(id: Order.Id, address: Address): StandardRoute = complete {
    askMapAddressActor[AddressActor.Reply.GetOrdersStatusInfo](address, AddressActor.Query.GetOrderStatusInfo(id)) {
      _.maybeOrderStatusInfo match {
        case Some(oi) => SimpleResponse(HttpOrderBookHistoryItem.fromOrderInfo(id, oi))
        case None => InfoNotFound(error.OrderNotFound(id))
      }
    }
  }

  @Path("/orders/{address}/{orderId}#getOrderStatusByAddressAndIdWithKey")
  @ApiOperation(
    value = "Order Status Info by Address and ID without signature. Requires API Key",
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
  def getOrderStatusByAddressAndIdWithKey: Route =
    (path(AddressPM / OrderPM) & get) { (addressOrError, orderIdOrError) =>
      (measureResponse("getOrderStatusByAddressAndIdWithKey") & protect) {
        (withAuth & withUserPublicKeyOpt) {
          userPublicKey =>
            withAddress(addressOrError) { address =>
              withOrderId(orderIdOrError) { orderId =>
                userPublicKey match {
                  case Some(upk) if upk.toAddress != address => invalidUserPublicKey
                  case _ => getOrderStatusInfo(orderId, address)
                }
              }
            }
        }
      }
    }

  // https://github.com/OAI/OpenAPI-Specification/issues/146#issuecomment-117288707
  @Path("/orderbook/{publicKey}/{orderId}#getOrderStatusByPKAndIdWithSig")
  @ApiOperation(
    value = "Order Status Info by Public Key and ID",
    notes = "Get Status Info of the specified order for a given public key",
    httpMethod = "GET",
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
  def getOrderStatusByPKAndIdWithSig: Route =
    (path(PublicKeyPM / OrderPM) & get) { (publicKeyOrError, orderIdOrError) =>
      (measureResponse("getOrderStatusByPKAndIdWithSig") & protect) {
        withOrderId(orderIdOrError) { orderId =>
          withPublicKey(publicKeyOrError) { publicKey =>
            signedGet(publicKey) {
              getOrderStatusInfo(orderId, publicKey.toAddress)
            }
          }
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}#getTradableBalanceByAssetPairAndAddress")
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
  def getTradableBalanceByAssetPairAndAddress: Route =
    (path(AssetPairPM / "tradableBalance" / AddressPM) & get) { (pairOrError, addressOrError) =>
      (measureResponse("getTradableBalanceByAssetPairAndAddress") & protect) {
        withAddress(addressOrError) { address =>
          withAssetPair(pairOrError, redirectToInverse = true, s"/tradableBalance/$address") { pair =>
            complete {
              askMapAddressActor[AddressActor.Reply.GetBalance](address, AddressActor.Query.GetTradableBalance(pair.assets))(_.balance.toJson)
            }
          }
        }
      }
    }

  @Path("/balance/reserved/{publicKey}#getReservedBalanceByPK")
  @ApiOperation(
    value = "Reserved Balance. Requires API Key if signature isn't provided",
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
  def getReservedBalanceByPK: Route = (path("reserved" / PublicKeyPM) & get) { publicKeyOrError =>
    (measureResponse("getReservedBalanceByPK") & protect) {
      withPublicKey(publicKeyOrError) { publicKey =>
        (signedGet(publicKey).tmap(_ => Option.empty[PublicKey]) | (withAuth & withUserPublicKeyOpt)) {
          case Some(upk) if upk != publicKey => invalidUserPublicKey
          case _ =>
            complete {
              askMapAddressActor[AddressActor.Reply.GetBalance](publicKey, AddressActor.Query.GetReservedBalance)(_.balance.toJson)
            }
        }
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/{orderId}#orderStatusByAssetPairAndId")
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
  def orderStatusByAssetPairAndId: Route = (path(AssetPairPM / OrderPM) & get) { (pairOrError, orderIdOrError) =>
    (measureResponse("orderStatusByAssetPairAndId") & protect) {
      withOrderId(orderIdOrError) { orderId =>
        withAssetPair(pairOrError, redirectToInverse = true, s"/$orderId") { _ =>
          val future =
            for {
              maybeOrder <- orderDb.get(orderId)
              result <- {
                maybeOrder match {
                  case Some(order) =>
                    askMapAddressActor[AddressActor.Reply.GetOrderStatus](order.sender, AddressActor.Query.GetOrderStatus(orderId)) { r =>
                      HttpOrderStatus.from(r.x)
                    }
                  case None =>
                    orderDb
                      .getOrderInfo(orderId)
                      .map(_.fold(HttpOrderStatus.from(OrderStatus.NotFound))(x => HttpOrderStatus.from(x.status)))
                      .map(ToResponseMarshallable(_))
                }
              }
            } yield result

          complete {
            future.recover { case th =>
              log.error("error while retrieving order status", th)
              ToResponseMarshallable(entities.InternalError)
            }
          }
        }
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}#deleteOrderBookWithKey")
  @ApiOperation(
    value = "Remove Order Book for a given Asset Pair. Requires API Key",
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
  def deleteOrderBookWithKey: Route =
    (path(AssetPairPM) & delete) { pairOrError =>
      (measureResponse("deleteOrderBookWithKey") & protect & withAuth) {
        withAssetPair(pairOrError, validate = false) { pair =>
          orderBook(pair) match {
            case Some(Right(_)) =>
              complete(
                storeCommand(ValidatedCommand.DeleteOrderBook(pair))
                  .map {
                    case None => NotImplemented(error.FeatureDisabled)
                    case _ => SimpleResponse(StatusCodes.Accepted, "Deleting order book")
                  }
                  .recover { case e: Throwable =>
                    log.error("Can not persist event", e)
                    CanNotPersist(error.CanNotPersistEvent)
                  }
              )
            case _ => complete(OrderBookUnavailable(error.OrderBookBroken(pair)))
          }
        }
      }
    }

  @Path("/transactions/{orderId}#getTransactionsByOrderId")
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
  def getTransactionsByOrderId: Route =
    (path(OrderPM) & get) { orderIdOrError =>
      (measureResponse("getTransactionsByOrderId") & protect) {
        withOrderId(orderIdOrError) { orderId =>
          complete {
            orderDb.transactionsByOrder(orderId).map(x => ToResponseMarshallable(Json.toJson(x))).recover {
              case th =>
                log.error("error while retrieving order transactions", th)
                ToResponseMarshallable(entities.InternalError)
            }
          }
        }
      }
    }

  @Path("/debug/config#getMatcherConfig")
  @ApiOperation(
    value = "Returns current matcher's configuration. Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    produces = "application/hocon",
    response = classOf[HttpResponse]
  )
  def getMatcherConfig: Route =
    (path("config") & get) {
      (measureResponse("getMatcherConfig") & withAuth) {
        complete {
          HttpEntity(safeConfig.rendered).withContentType(CustomContentTypes.`application/hocon`)
        }
      }
    }

  @Path("/debug/currentOffset#getCurrentOffset")
  @ApiOperation(
    value = "Get the current offset in the queue. Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getCurrentOffset: Route =
    (path("currentOffset") & get) {
      (measureResponse("getCurrentOffset") & withAuth) {
        complete(currentOffset().toJson)
      }
    }

  @Path("/debug/lastOffset#getLastOffset")
  @ApiOperation(
    value = "Get the last offset in the queue. Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getLastOffset: Route =
    (path("lastOffset") & get) {
      (measureResponse("getLastOffset") & withAuth) {
        complete(lastOffset() map (_.toJson))
      }
    }

  @Path("/debug/oldestSnapshotOffset#getOldestSnapshotOffset")
  @ApiOperation(
    value = "Get the oldest snapshot's offset in the queue. Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpOffset]
  )
  def getOldestSnapshotOffset: Route =
    (path("oldestSnapshotOffset") & get) {
      (measureResponse("getOldestSnapshotOffset") & withAuth) {
        complete {
          (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { response =>
            val defined = response.offsets.valuesIterator.collect { case Some(x) => x }
            (if (defined.isEmpty) -1L else defined.min).toJson
          }
        }
      }
    }

  @Path("/debug/allSnapshotOffsets#getAllSnapshotOffsets")
  @ApiOperation(
    value = "Get all snapshots' offsets in the queue. Requires API Key",
    notes = "Returns Map[Asset Pair, Long]",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpSnapshotOffsets]
  )
  def getAllSnapshotOffsets: Route =
    (path("allSnapshotOffsets") & get) {
      (measureResponse("getAllSnapshotOffsets") & withAuth) {
        complete {
          (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { x =>
            x.offsets.collect { case (assetPair, Some(offset)) => assetPair -> offset }.toJson
          }
        }
      }
    }

  @Path("/debug/saveSnapshots#saveSnapshots")
  @ApiOperation(
    value = "Saves snapshots for all Order Books. Requires API Key",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpMessage]
  )
  def saveSnapshots: Route =
    (path("saveSnapshots") & post) {
      (measureResponse("saveSnapshots") & protect & withAuth) {
        complete {
          matcher ! ForceSaveSnapshots
          SimpleResponse(StatusCodes.OK, "Saving started")
        }
      }
    }

  @Path("/debug/address/{address}#getAddressState")
  @ApiOperation(
    value = "Get state (balances, placement queue by address). Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpAddressState]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path")
    )
  )
  def getAddressState: Route =
    (path("address" / AddressPM) & get) { addressOrError =>
      (measureResponse("getAddressState") & withAuth) {
        withAddress(addressOrError) { address =>
          complete {
            askMapAddressActor[GetState](address, GetCurrentState) {
              HttpAddressState(_)
            }
          }
        }
      }
    }

  @Path("/debug/status#getMatcherStatus")
  @ApiOperation(
    value = "Returns current matcher's status. Requires API Key",
    httpMethod = "GET",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("debug"),
    response = classOf[HttpSystemStatus]
  )
  def getMatcherStatus: Route =
    (path("status") & get) {
      (measureResponse("getMatcherStatus") & withAuth) {
        complete(HttpSystemStatus(matcherStatus(), blockchainStatus))
      }
    }

  // Hidden
  def printMessage: Route =
    (path("print") & post) {
      (measureResponse("printMessage") & withAuth) {
        entity(as[HttpMessage]) { x =>
          log.warn(x.message)
          complete {
            SimpleResponse(StatusCodes.OK, "Message logged")
          }
        }
      }
    }

  private def askMapAddressActor[A: ClassTag](sender: Address, msg: AddressActor.Message)(
    f: A => ToResponseMarshallable
  ): Future[ToResponseMarshallable] =
    (addressActor ? AddressDirectoryActor.Command.ForwardMessage(sender, msg))
      .mapTo[A]
      .map(f)
      .recover { case e: AskTimeoutException =>
        log.error(s"Error processing $msg", e)
        TimedOut
      }

  private val handleUnknownResponse: LogicResponseHandler = { case x =>
    log.error(s"Can't handle $x")
    entities.InternalError
  }

  @inline
  private def askAddressActor(sender: Address, msg: AddressActor.Message)(handleResponse: LogicResponseHandler): Future[ToResponseMarshallable] =
    (addressActor ? AddressDirectoryActor.Command.ForwardMessage(sender, msg))
      .map(handleResponse.orElse(handleUnknownResponse))
      .recover { case e: AskTimeoutException =>
        log.error(s"Error processing $msg", e)
        TimedOut
      }

  private def getOrderListType(activeOnly: Option[Boolean], closedOnly: Option[Boolean], default: OrderListType): OrderListType =
    if (activeOnly.isEmpty && closedOnly.isEmpty) default
    else
      (activeOnly.getOrElse(false), closedOnly.getOrElse(false)) match {
        case (true, true) => OrderListType.Empty
        case (false, true) => OrderListType.ClosedOnly
        case (true, false) => OrderListType.ActiveOnly
        case (false, false) => OrderListType.All
      }

}
