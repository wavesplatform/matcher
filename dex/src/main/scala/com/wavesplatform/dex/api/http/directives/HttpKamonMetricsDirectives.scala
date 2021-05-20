package com.wavesplatform.dex.api.http.directives

import akka.http.scaladsl.model.AttributeKey
import akka.http.scaladsl.server.Directives.{extractRequest, mapResponse}
import kamon.Kamon
import kamon.metric.{Counter, Timer}

object HttpKamonMetricsDirectives {

  val requestMetricsAttributeKey: AttributeKey[RequestMetrics] = AttributeKey[RequestMetrics]("reqMetrics")

  private val timer = Kamon.timer("matcher.http.endpoints.timer")
  private val counter = Kamon.counter("matcher.http.responses.counter")

  def measureResponse(endpoint: String) = extractRequest.tflatMap { req =>
    val path = req._1.uri.path.toString()
    val method = req._1.method.value
    val startedTimer = timer.withTag("path", path).withTag("endpoint", endpoint).withTag("method", method).start()
    val taggedCounter = counter.withTag("path", path).withTag("endpoint", endpoint).withTag("method", method)
    val metrics = RequestMetrics(taggedCounter, startedTimer)
    mapResponse(_.addAttribute(requestMetricsAttributeKey, metrics))
  }

}

case class RequestMetrics(counter: Counter, startedTimer: Timer.Started)
