package com.wavesplatform.dex.api.http.routes

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.directives.FutureDirectives
import akka.http.scaladsl.server.{Directive1, Route}
import com.wavesplatform.dex.Matcher
import com.wavesplatform.dex.api.http.entities.{HttpV1OrderBook, InfoNotFound}
import com.wavesplatform.dex.api.http.{HasStatusBarrier, OrderBookHttpInfo}
import com.wavesplatform.dex.api.routes.{ApiRoute, AuthRoute, PathMatchers}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.model.{AssetPairBuilder, MatcherModel}
import io.swagger.annotations.{Api, ApiImplicitParam, ApiImplicitParams, ApiOperation}
import javax.ws.rs.Path

@Path("/api/v1")
@Api(value = "/api v1/")
case class MatcherApiRouteV1(assetPairBuilder: AssetPairBuilder,
                             orderBookHttpInfo: OrderBookHttpInfo,
                             matcherStatus: () => Matcher.Status,
                             apiKeyHash: Option[Array[Byte]])(implicit val errorContext: ErrorFormatterContext)
    extends ApiRoute
    with AuthRoute
    with HasStatusBarrier
    with ScorexLogging {

  import PathMatchers._

  override lazy val route: Route = pathPrefix("api" / "v1") {
    matcherStatusBarrier {
      getOrderBook
    }
  }

  private def withAssetPair(p: AssetPair,
                            redirectToInverse: Boolean,
                            suffix: String = "",
                            formatError: MatcherError => ToResponseMarshallable = InfoNotFound.apply): Directive1[AssetPair] = {
    FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p).value } flatMap {
      case Right(_) => provide(p)
      case Left(e) if redirectToInverse =>
        FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p.reverse).value } flatMap {
          case Right(_) => redirect(s"/api/v1/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
          case Left(_)  => complete(formatError(e))
        }
      case Left(e) => complete { formatError(e) }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(
    value = "Get Order Book for a given Asset Pair",
    notes = "Get Order Book for a given Asset Pair",
    httpMethod = "GET",
    response = classOf[HttpV1OrderBook]
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
    parameters(Symbol("depth").as[Int].?) { depth =>
      withAssetPair(p, redirectToInverse = true) { pair =>
        complete { orderBookHttpInfo.getHttpView(pair, MatcherModel.Denormalized, depth) }
      }
    }
  }
}
