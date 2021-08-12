package com.wavesplatform.dex.api.http.routes.v0

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.dex._
import com.wavesplatform.dex.actors.OrderBookDirectoryActor._
import com.wavesplatform.dex.actors.address.AddressActor
import com.wavesplatform.dex.api.http.directives.HttpKamonDirectives._
import com.wavesplatform.dex.api.http.directives.ProtectDirective
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.headers.`X-User-Public-Key`
import com.wavesplatform.dex.api.http.{HasStatusBarrier, OrderBookHttpInfo, _}
import com.wavesplatform.dex.api.routes.PathMatchers.{AddressPM, AssetPairPM, OrderPM, PublicKeyPM}
import com.wavesplatform.dex.api.routes.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.db.OrderDb
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.model.{AssetPairBuilder, _}
import com.wavesplatform.dex.queue.MatcherQueue.StoreValidatedCommand
import com.wavesplatform.dex.queue.ValidatedCommand
import com.wavesplatform.dex.settings.{OrderRestrictionsSettings}
import io.swagger.annotations._

import javax.ws.rs.Path
import scala.concurrent.{ExecutionContext, Future}

@Path("/matcher")
@Api()
final class MarketsRoute(
  settings: MarketsRoute.Settings,
  addressActor: ActorRef,
  orderDb: OrderDb[Future],
  assetPairBuilder: AssetPairBuilder,
  matcherPublicKey: PublicKey,
  matcher: ActorRef,
  storeCommand: StoreValidatedCommand,
  orderBook: AssetPair => Option[Either[Unit, ActorRef]],
  orderBookHttpInfo: OrderBookHttpInfo,
  override val matcherStatus: () => MatcherStatus,
  override val apiKeyHash: Option[Array[Byte]]
)(implicit mat: Materializer)
    extends ApiRoute
    with ProtectDirective
    with HasStatusBarrier
    with AuthRoute
    with ScorexLogging {

  implicit private val executionContext: ExecutionContext = mat.executionContext
  implicit private val timeout: Timeout = settings.timeout

  // NOTE: routes here must not change their places (especially getOrderBookRestrictions ~ getOrderStatusByPKAndIdWithSig ~ getOrderBook)
  override lazy val route: Route =
    pathPrefix("matcher") {
      pathPrefix("orderbook") {
        matcherStatusBarrier {
          getOrderBookRestrictions ~ getOrderStatusByPKAndIdWithSig ~ getOrderBook ~ getOrderBooks ~ getOrderBookStatus ~ deleteOrderBookWithKey ~ getOrderStatusByAssetPairAndId
        }
      } ~ pathPrefix("orders") {
        matcherStatusBarrier(getOrderStatusByAddressAndIdWithKey)
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
      (withMetricsAndTraces("getOrderBook") & protect) {
        parameters("depth".as[String].?) {
          case None => withAssetPair(assetPairBuilder, pairOrError, redirectToInverse = true, "") { pair =>
              complete(orderBookHttpInfo.getHttpView(pair, MatcherModel.Normalized, None))
            }
          case Some(depth) =>
            depth.toIntOption match {
              case None => complete(InvalidDepth(s"Depth value '$depth' must be an Integer"))
              case Some(d) =>
                if (d >= 0) withAssetPair(assetPairBuilder, pairOrError, redirectToInverse = true, s"?depth=$d") { pair =>
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
      (withMetricsAndTraces("getOrderBookStatus") & protect) {
        withAssetPair(assetPairBuilder, pairOrError, redirectToInverse = true, suffix = "/status") { pair =>
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
      (withMetricsAndTraces("getOrderBookRestrictions") & protect) {
        withAssetPair(assetPairBuilder, pairOrError, redirectToInverse = true, suffix = "/info") { pair =>
          complete(getOrderBookRestrictions(pair).value.map {
            case Right(x) => SimpleResponse(x)
            case Left(e) => InfoNotFound(e)
          })
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
      (withMetricsAndTraces("getOrderStatusByAddressAndIdWithKey") & protect) {
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
      (withMetricsAndTraces("getOrderStatusByPKAndIdWithSig") & protect) {
        withOrderId(orderIdOrError) { orderId =>
          withPublicKey(publicKeyOrError) { publicKey =>
            signedGet(publicKey) {
              getOrderStatusInfo(orderId, publicKey.toAddress)
            }
          }
        }
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/{orderId}#getOrderStatusByAssetPairAndId")
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
  def getOrderStatusByAssetPairAndId: Route = (path(AssetPairPM / OrderPM) & get) { (pairOrError, orderIdOrError) =>
    (withMetricsAndTraces("getOrderStatusByAssetPairAndId") & protect) {
      withOrderId(orderIdOrError) { orderId =>
        withAssetPair(assetPairBuilder, pairOrError, redirectToInverse = true, s"/$orderId") { _ =>
          val future =
            for {
              maybeOrder <- orderDb.get(orderId)
              result <- {
                maybeOrder match {
                  case Some(order) =>
                    askMapAddressActor[AddressActor.Reply.GetOrderStatus](addressActor, order.sender, AddressActor.Query.GetOrderStatus(orderId)) {
                      r =>
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
      (withMetricsAndTraces("getOrderBooks") & protect) {
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
      (withMetricsAndTraces("deleteOrderBookWithKey") & protect & withAuth) {
        withAssetPair(assetPairBuilder, pairOrError, validate = false) { pair =>
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

  private def getOrderBookRestrictions(pair: AssetPair): FutureResult[HttpOrderBookInfo] = settings.getActualTickSize(pair).map { tickSize =>
    HttpOrderBookInfo(
      restrictions = settings.orderRestrictions.get(pair).map(HttpOrderRestrictions.fromSettings),
      matchingRules = HttpMatchingRules(tickSize = tickSize.toDouble)
    )
  }

  private def getOrderStatusInfo(id: Order.Id, address: Address): StandardRoute = complete {
    askMapAddressActor[AddressActor.Reply.GetOrdersStatusInfo](addressActor, address, AddressActor.Query.GetOrderStatusInfo(id)) {
      _.maybeOrderStatusInfo match {
        case Some(oi) => SimpleResponse(HttpOrderBookHistoryItem.fromOrderInfo(id, oi))
        case None => InfoNotFound(error.OrderNotFound(id))
      }
    }
  }

}

object MarketsRoute {

  final case class Settings(
    timeout: Timeout,
    getActualTickSize: AssetPair => FutureResult[BigDecimal],
    orderRestrictions: Map[AssetPair, OrderRestrictionsSettings]
  )

}
