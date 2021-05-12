package com.wavesplatform.dex.api.http.directives

import akka.http.scaladsl.model.AttributeKey
import akka.http.scaladsl.server.Directives.{attribute, mapResponse}
import akka.http.scaladsl.server.{Directive0, Directives}
import com.wavesplatform.dex.api.http.directives.HttpKamonMetricsDirectives._

import java.util.UUID

trait HttpKamonMetricsProtectedDirectives extends Directives with ProtectDirective {

  def protectedMeasureResponses(endpoint: String): Directive0 =
    measureResponse(endpoint) & protect

  def statusBarrierMeasureResponses(endpoint: String): Directive0 =
    measureResponse(endpoint) & matcherStatusBarrier

}

object HttpKamonMetricsDirectives {

  val endpointAttributeKey: AttributeKey[String] = AttributeKey[String]("endpoint")
  val requestIdAttributeKey: AttributeKey[UUID] = AttributeKey("req-id", classOf[UUID])

  def measureResponse(endpoint: String): Directive0 =
    mapResponse(_.addAttribute(endpointAttributeKey, endpoint))

  def passRequestIdAttributeToResponse: Directive0 = passAttributeToResponse(requestIdAttributeKey)

  def passAttributeToResponse[T](attrKey: AttributeKey[T]): Directive0 =
    attribute(attrKey).flatMap { id =>
      mapResponse(_.addAttribute(attrKey, id))
    }

}
