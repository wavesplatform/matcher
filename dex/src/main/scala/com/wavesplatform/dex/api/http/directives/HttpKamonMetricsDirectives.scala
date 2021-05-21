package com.wavesplatform.dex.api.http.directives

import akka.http.scaladsl.model.AttributeKey
import akka.http.scaladsl.server.Directives.{extractRequest, mapResponse}
import kamon.Kamon
import kamon.metric.{Counter, Timer}

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

object HttpKamonMetricsDirectives {

  val requestMetricsAttributeKey: AttributeKey[RequestMetrics] = AttributeKey[RequestMetrics]("reqMetrics")

  private val timer = Kamon.timer("matcher.http.endpoints.timer")
  private val counter = Kamon.counter("matcher.http.responses.counter")

  val ec = new ScheduledThreadPoolExecutor(1)

  def measureResponse(endpoint: String) = extractRequest.tflatMap { req =>
    val path = req._1.uri.path.toString()
    val method = req._1.method.value
    val startedTimer = timer.withTag("path", path).withTag("endpoint", endpoint).withTag("method", method)
    val taggedCounter = counter.withTag("path", path).withTag("endpoint", endpoint).withTag("method", method)
    ec.schedule(
      new Runnable {
        override def run(): Unit = {
          startedTimer.asInstanceOf[kamon.metric.Instrument[_, _]].remove()
          taggedCounter.asInstanceOf[kamon.metric.Instrument[_, _]].remove()
        }
      },
      60,
      TimeUnit.SECONDS
    )
    val metrics = RequestMetrics(taggedCounter, startedTimer.start())
    mapResponse(_.addAttribute(requestMetricsAttributeKey, metrics))
  }

}

case class RequestMetrics(counter: Counter, startedTimer: Timer.Started)
