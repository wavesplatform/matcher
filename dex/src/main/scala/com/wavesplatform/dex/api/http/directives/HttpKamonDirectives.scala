package com.wavesplatform.dex.api.http.directives

import akka.http.scaladsl.model.AttributeKey
import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.server.Directives.{extractRequest, mapResponse}
import com.wavesplatform.dex.tool.KamonTraceUtils
import kamon.Kamon
import kamon.metric.{Counter, Timer}
import kamon.tag.TagSet

object HttpKamonDirectives {

  val requestMetricsAttributeKey: AttributeKey[RequestMetrics] = AttributeKey[RequestMetrics]("reqMetrics")

  private val timer = Kamon.timer("matcher.http.endpoints.timer")
  private val counter = Kamon.counter("matcher.http.responses.counter")

  def withMetricsAndTraces(endpoint: String): Directive[Unit] =
    withTraces(endpoint).tflatMap(_ => withMetrics(endpoint))

  def withTraces(endpoint: String): Directive[Unit] =
    extractRequest.tmap(_ => KamonTraceUtils.setSpanNameAndForceSamplingDecision("/" + endpoint))

  def withMetrics(endpoint: String): Directive[Unit] = extractRequest.tflatMap { req =>
    val method = req._1.method.value
    val tagset =
      TagSet.from(Map(
        "endpoint" -> endpoint,
        "method" -> method
      ))
    val startedTimer = timer.withTags(tagset).start()
    val taggedCounter = counter.withTags(tagset)
    val metrics = RequestMetrics(taggedCounter, startedTimer)
    mapResponse(_.addAttribute(requestMetricsAttributeKey, metrics))
  }

}

case class RequestMetrics(counter: Counter, startedTimer: Timer.Started)
