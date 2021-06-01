package com.wavesplatform.dex.api.http

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import akka.http.scaladsl.server.{Directive0, Route, RouteResult}
import com.wavesplatform.dex.Application
import com.wavesplatform.dex.api.routes.ApiRoute
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.RestAPISettings
import com.wavesplatform.dex.tool.KamonTraceUtils

class CompositeHttpService(apiTypes: Set[Class[_]], routes: Seq[ApiRoute], settings: RestAPISettings) extends ScorexLogging {

  private val swaggerService = new SwaggerDocService(apiTypes, s"${settings.address}:${settings.port}")
  private val redirectToSwagger = redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)

  private val swaggerRoute: Route = {
    swaggerService.routes ~
    (pathEndOrSingleSlash | path("swagger"))(redirectToSwagger) ~
    pathPrefix("api-docs") {
      pathEndOrSingleSlash(redirectToSwagger) ~
      getFromResourceDirectory(s"META-INF/resources/webjars/swagger-ui/${SwaggerUiVersion.VersionString}", Application.getClass.getClassLoader)
    }
  }

  private val notFound: Route = complete(StatusCodes.NotFound)

  val compositeRoute: Route =
    mapInnerRoute { route => ctx =>
      //initially span name is "/rejected" and will be overridden in subsequent routes
      //"/rejected" spans will be filtered out by FilteringRejectedHook
      KamonTraceUtils.setSpanName("/rejected")
      route(ctx)
    }(extendRoute(concat(routes.map(_.route): _*)) ~ swaggerRoute ~ notFound)

  val loggingCompositeRoute: Route = DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logRequestResponse))(compositeRoute)

  private def logRequestResponse(req: HttpRequest)(res: RouteResult): Unit = res match {
    case Complete(resp) =>
      val msg = s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}"
      if (resp.status == StatusCodes.OK) log.info(msg) else log.warn(msg)
    case _ =>
  }

  private val corsAllowedHeaders = (if (settings.apiKeyDifferentHost) List("api_key", "X-API-Key") else List.empty[String]) ++
    Seq("Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature")

  private def corsAllowAll: Directive0 = if (settings.cors) respondWithHeader(`Access-Control-Allow-Origin`.*) else pass

  private def extendRoute(base: Route): Route =
    if (settings.cors) { ctx =>
      val extendedRoute = corsAllowAll(base) ~ options {
        respondWithDefaultHeaders(
          `Access-Control-Allow-Credentials`(true),
          `Access-Control-Allow-Headers`(corsAllowedHeaders),
          `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE)
        )(corsAllowAll(complete(StatusCodes.OK)))
      }

      extendedRoute(ctx)
    } else base

}
