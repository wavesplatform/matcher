package com.wavesplatform.dex.api

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCodes => C, _}
import akka.util.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.util.getSimpleName
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.byteStrWrites
import play.api.libs.json._

sealed class MatcherResponse(val statusCode: StatusCode, val content: MatcherResponseContent) {
  def this(code: StatusCode, error: MatcherError) = this(code, MatcherResponseContent.Error(error))
  def this(code: StatusCode, error: JsObject) = this(code, MatcherResponseContent.Js(error))

  def status: String = getSimpleName(this)
}

object MatcherResponse {
  def toResponseMarshaller(implicit context: ErrorFormatterContext): ToResponseMarshaller[MatcherResponse] = Marshaller.opaque { x =>
    def content(x: MatcherResponse): JsValue =
      backwardCompatibleWrapper(
        x.status,
        x.statusCode,
        x.content match {
          case MatcherResponseContent.Js(r)        => r
          case MatcherResponseContent.Error(error) => error.toJson(context)
          case MatcherResponseContent.Multiple(xs) => Json.obj("message" -> Json.arr(xs.map(content)))
        }
      )

    HttpResponse(
      x.statusCode,
      entity = HttpEntity.Strict(ContentTypes.`application/json`, ByteString(Json.stringify(content(x))))
    )
  }

  private def backwardCompatibleWrapper(status: String, code: StatusCode, json: JsObject): JsValue =
    Json
      .obj(
        "success" -> (code match {
          case _: C.Success => true
          case _            => false
        }),
        "status" -> status
      )
      .deepMerge(code match {
        case _: C.Success => JsObject.empty
        case _            => Json.obj("result" -> JsNull) // For a backward compatibility
      })
      .deepMerge(json)
}

sealed trait MatcherResponseContent
object MatcherResponseContent {
  case class Js(content: JsObject)                    extends MatcherResponseContent
  case class Error(content: MatcherError)             extends MatcherResponseContent
  case class Multiple(content: List[MatcherResponse]) extends MatcherResponseContent
}

case class SimpleResponse(code: StatusCode, message: String) extends MatcherResponse(code, Json.obj("message" -> message))
case class InvalidJsonResponse(error: MatcherError)          extends MatcherResponse(C.BadRequest, error)
case object AlreadyProcessed                                 extends MatcherResponse(C.Accepted, Json.obj("message" -> "This event has been already processed"))
case class OrderAccepted(order: Order)                       extends MatcherResponse(C.OK, Json.obj("message" -> order.json()))
case class OrderCanceled(orderId: ByteStr)                   extends MatcherResponse(C.OK, Json.obj("orderId" -> orderId))
case class OrderDeleted(orderId: ByteStr)                    extends MatcherResponse(C.OK, Json.obj("orderId" -> orderId))

case class BatchCancelCompleted(orders: Map[Order.Id, MatcherResponse])
    extends MatcherResponse(C.OK, MatcherResponseContent.Multiple(orders.values.toList))

case class OrderRejected(error: MatcherError)                              extends MatcherResponse(C.BadRequest, error)
case class OrderCancelRejected(error: MatcherError)                        extends MatcherResponse(C.BadRequest, error)
case object InvalidSignature                                               extends MatcherResponse(C.BadRequest, error.RequestInvalidSignature)
case class NotImplemented(error: MatcherError)                             extends MatcherResponse(C.NotImplemented, error)
case class OrderBookUnavailable(error: MatcherError)                       extends MatcherResponse(C.ServiceUnavailable, error)
case object DuringStart                                                    extends MatcherResponse(C.ServiceUnavailable, error.MatcherIsStarting)
case object DuringShutdown                                                 extends MatcherResponse(C.ServiceUnavailable, error.MatcherIsStopping)
case object TimedOut                                                       extends MatcherResponse(C.InternalServerError, error.RequestTimeout)
case class InfoNotFound(error: MatcherError)                               extends MatcherResponse(C.NotFound, error)
case class RateError(error: MatcherError, code: StatusCode = C.BadRequest) extends MatcherResponse(code, error)
case object InternalError                                                  extends MatcherResponse(C.ServiceUnavailable, MatcherResponseContent.Js(Json.obj("message" -> "Internal server error")))
