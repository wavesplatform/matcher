package com.wavesplatform.dex.api.http.routes.v0

import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.pattern.CircuitBreakerOpenException
import akka.stream.Materializer
import cats.syntax.either._
import com.wavesplatform.dex._
import com.wavesplatform.dex.api.http.SwaggerDocService
import com.wavesplatform.dex.api.http.directives.HttpKamonDirectives._
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.routes.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.caches.OrderFeeSettingsCache
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.queue.MatcherQueue.StoreValidatedCommand
import com.wavesplatform.dex.queue.ValidatedCommand
import io.swagger.annotations._

import javax.ws.rs.Path
import scala.concurrent.{ExecutionContext, TimeoutException}
import scala.util.{Failure, Success}

@Path("/matcher")
@Api()
final class CustomAssetsFeeRoute(
  override val apiKeyHashes: List[Array[Byte]],
  orderFeeSettingsCache: OrderFeeSettingsCache,
  store: StoreValidatedCommand
)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  implicit private val executionContext: ExecutionContext = mat.executionContext

  override lazy val route: Route =
    pathPrefix("matcher" / "settings" / "custom-fee-assets")(addCustomFeeAssets ~ deleteCustomFeeAssets)

  @Path("/settings/custom-fee-assets#addCustomFeeAssets")
  @ApiOperation(
    value = "Add Custom Fee Assets",
    notes = "Add Custom Fee Assets",
    httpMethod = "POST",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("custom-fee-assets"),
    response = classOf[HttpMessage]
  )
  def addCustomFeeAssets: Route =
    (pathEndOrSingleSlash & post & withAuth) {
      entity(as[HttpCustomFeeAssets]) { customFeeAssets =>
        withMetricsAndTraces("addCustomFeeAssets") {
          handleCustomAssetEvent(
            customFeeAssets.assets,
            "addCustomFeeAssets",
            asset => !orderFeeSettingsCache.containsCustomFeeAsset(asset),
            assets => ValidatedCommand.AddCustomAssetToFee(assets)
          )
        }
      }
    }

  @Path("/settings/custom-fee-assets#deleteCustomFeeAssets")
  @ApiOperation(
    value = "Delete Custom Fee Assets",
    notes = "Delete Custom Fee Assets",
    httpMethod = "DELETE",
    authorizations = Array(new Authorization(SwaggerDocService.apiKeyDefinitionName)),
    tags = Array("custom-fee-assets"),
    response = classOf[HttpMessage]
  )
  def deleteCustomFeeAssets: Route =
    (pathEndOrSingleSlash & delete & withAuth) {
      entity(as[HttpCustomFeeAssets]) { customFeeAssets =>
        withMetricsAndTraces("deleteCustomFeeAssets") {
          handleCustomAssetEvent(
            customFeeAssets.assets,
            "deleteCustomFeeAssets",
            asset => orderFeeSettingsCache.containsCustomFeeAsset(asset),
            assets => ValidatedCommand.DeleteCustomAssetToFee(assets)
          )
        }
      }
    }

  private def handleCustomAssetEvent(
    assets: Set[Asset],
    actionName: String,
    predicate: Asset => Boolean,
    cmdConstructor: Set[Asset] => ValidatedCommand
  ): StandardRoute = {
    val assetsToHandle = assets.filter(predicate)
    if (assetsToHandle.isEmpty)
      complete(HttpMessage(s"There is no assets to do $actionName"))
    else {
      val cmd = cmdConstructor(assetsToHandle)
      complete(
        store(cmd).transform {
          case Success(None) => Success(error.FeatureDisabled.asLeft[HttpMessage])
          case Success(_) => Success(HttpMessage(s"Successfully saved command for $actionName - $assetsToHandle").asRight[MatcherError])
          case Failure(e) =>
            val prefix = s"Store failed for $actionName - $assetsToHandle"
            log.warn(
              e match {
                case _: TimeoutException => s"$prefix: timeout during storing $actionName - $assetsToHandle"
                case _: CircuitBreakerOpenException => s"$prefix: fail fast due to circuit breaker"
                case _ => prefix
              },
              e
            )
            Success(error.CanNotPersistEvent.asLeft[HttpMessage])
        }
      )
    }
  }

}
