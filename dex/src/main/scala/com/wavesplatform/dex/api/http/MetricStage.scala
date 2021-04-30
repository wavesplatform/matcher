package com.wavesplatform.dex.api.http

import akka.NotUsed
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.{BidiFlow, Flow}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, BidiShape, Inlet, Outlet}
import com.wavesplatform.dex.api.http.directives.HttpKamonMetricsDirectives._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import kamon.Kamon
import kamon.metric.{Metric, Timer}

import java.util.UUID
import scala.collection.mutable

class MetricStage() extends GraphStage[BidiShape[HttpRequest, HttpRequest, HttpResponse, HttpResponse]] with ScorexLogging {

  private val requestIn = Inlet[HttpRequest]("MetricStage.requestIn")
  private val requestOut = Outlet[HttpRequest]("MetricStage.requestOut")
  private val responseIn = Inlet[HttpResponse]("MetricStage.responseIn")
  private val responseOut = Outlet[HttpResponse]("MetricStage.responseOut")

  override def initialAttributes: Attributes = Attributes.name("MetricStage")

  val shape = new BidiShape(requestIn, requestOut, responseIn, responseOut)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {

    private val pending = mutable.Map.empty[UUID, RequestWithTimer]

    val requestHandler = new InHandler with OutHandler {

      override def onPush(): Unit = {
        val request = grab(requestIn)
        request.attribute(requestIdAttributeKey) match {
          case Some(id) =>
            val startedTimer = timer.withoutTags().start()
            pending += id -> RequestWithTimer(request, startedTimer)
            push(requestOut, request)
          case _ =>
            failStage(NoRequestIdAttributeFound)
        }
      }

      override def onPull(): Unit =
        if (!isClosed(requestIn) && !hasBeenPulled(requestIn))
          pull(requestIn)

      override def onUpstreamFinish(): Unit = complete(requestOut)
      override def onUpstreamFailure(ex: Throwable): Unit = fail(requestOut, ex)
      override def onDownstreamFinish(cause: Throwable): Unit = cancel(requestIn)
    }

    val responseHandler = new InHandler with OutHandler {

      override def onPush(): Unit = {
        val response: HttpResponse = grab(responseIn)
        response.attribute(requestIdAttributeKey) match {
          case Some(id) =>
            pending.remove(id) match {
              case Some(pendingData) =>
                push(responseOut, postHandle(pendingData.req, response, pendingData.startedTimer))

              case None =>
                failStage(NoRequestWithIdFound(id))
            }

          case _ =>
            failStage(NoRequestIdAttributeFound)
        }

      }

      override def onPull(): Unit = pull(responseIn)

      override def onUpstreamFinish(): Unit =
        complete(responseOut)

      override def onUpstreamFailure(ex: Throwable): Unit = {
        fail(responseOut, ex)
      }

      override def onDownstreamFinish(cause: Throwable): Unit = {
        cancel(responseIn)
      }

    }

    setHandlers(requestIn, requestOut, requestHandler)
    setHandlers(responseIn, responseOut, responseHandler)
  }

  private val counter = Kamon.counter("matcher.http.responses.counter")
  private val timer: Metric.Timer = Kamon.timer("matcher.http.endpoints.timer")

  private def postHandle(req: HttpRequest, resp: HttpResponse, startedTimer: Timer.Started): HttpResponse = {
    val url = req.uri.path.toString()
    val method = req.method.value
    successResponse(resp, startedTimer, method, url)
  }

  private def successResponse(resp: HttpResponse, startTimer: Timer.Started, method: String, url: String): HttpResponse = {
    val endpoint = getEndpoint(resp)
    startTimer.withTag("path", url).withTag("endpoint", endpoint).withTag("method", method).stop()
    getCounter(url, method, resp.status.intValue().toString, endpoint).increment()
    resp
  }

  private def getEndpoint(resp: HttpResponse): String =
    resp.attribute(endpointAttributeKey).getOrElse("noEndpoint")

  private def getCounter(path: String, method: String, status: String, endpoint: String) =
    counter.withTag("path", path).withTag("endpoint", endpoint).withTag("method", method).withTag("status", status)

}

object MetricStage {

  def metricFlow(): BidiFlow[HttpRequest, HttpRequest, HttpResponse, HttpResponse, NotUsed] = {
    val requestIdSet = BidiFlow.fromFlows(Flow[HttpRequest].map(addId), Flow[HttpResponse])
    val metric = BidiFlow.fromGraph(new MetricStage())
    requestIdSet.atop(metric)
  }

  def addId(req: HttpRequest): HttpRequest =
    req.addAttribute(requestIdAttributeKey, UUID.randomUUID())

}

object NoRequestIdAttributeFound extends Throwable
case class NoRequestWithIdFound(id: UUID) extends Throwable

case class RequestWithTimer(
  req: HttpRequest,
  startedTimer: Timer.Started
)
