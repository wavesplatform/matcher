package com.wavesplatform.dex.api

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import com.wavesplatform.api.http._
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.model.MatcherModel
import com.wavesplatform.dex.{AssetPairBuilder, Matcher}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import io.swagger.annotations._
import javax.ws.rs.Path

@Path("/api/v1")
@Api(value = "/api v1/")
case class MatcherApiRouteV1(assetPairBuilder: AssetPairBuilder,
                             orderBookSnapshot: OrderBookSnapshotHttpCache,
                             matcherStatus: () => Matcher.Status,
                             apiKeyHash: Option[Array[Byte]])(implicit val errorContext: ErrorFormatterContext)
    extends ApiRoute
    with ScorexLogging {

  import PathMatchers._

  private implicit val trm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller

  override lazy val route: Route = pathPrefix("api" / "v1") {
    matcherStatusBarrier {
      getOrderBook
    }
  }

  private def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }

  private def defaultFormatError(e: MatcherError): ToResponseMarshallable = InfoNotFound(e)

  private def withAssetPair(p: AssetPair,
                            redirectToInverse: Boolean,
                            suffix: String = "",
                            formatError: MatcherError => ToResponseMarshallable = defaultFormatError): Directive1[AssetPair] =
    assetPairBuilder.validateAssetPair(p) match {
      case Right(_) => provide(p)
      case Left(e) if redirectToInverse =>
        assetPairBuilder
          .validateAssetPair(p.reverse)
          .fold(
            _ => complete(formatError(e)),
            _ => redirect(s"/api/v1/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
          )
      case Left(e) => complete(formatError(e))
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(value = "Get Order Book for a given Asset Pair", notes = "Get Order Book for a given Asset Pair", httpMethod = "GET")
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
      withAssetPair(p, redirectToInverse = true) { pair =>
        complete(orderBookSnapshot.get(pair, depth, MatcherModel.Denormalized))
      }
    }
  }
}
