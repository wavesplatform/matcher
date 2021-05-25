package com.wavesplatform.dex.api.http

import akka.NotUsed
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Route
import akka.actor.ActorSystem
import akka.stream.scaladsl.Flow
import com.wavesplatform.dex.api.http.directives.HttpKamonDirectives.requestMetricsAttributeKey

object MetricHttpFlow {

  def metricFlow(combinedRoute: Route)(implicit as: ActorSystem): Flow[HttpRequest, HttpResponse, NotUsed] =
    Flow[HttpRequest].via(combinedRoute).via(metricFlow())

  def metricFlow(): Flow[HttpResponse, HttpResponse, NotUsed] = Flow[HttpResponse].wireTap { response =>
    response.attribute(requestMetricsAttributeKey).foreach { metrics =>
      val status = response.status.intValue()
      metrics.startedTimer.withTag("status", status).stop()
      metrics.counter.withTag("status", status).increment()
    }
  }

}
