package com.wavesplatform.dex.api

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCodes => C, _}
import akka.util.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.byteStrWrites
import play.api.libs.json._

sealed abstract class MatcherResponse(val statusCode: StatusCode) {
  def content: MatcherResponseContent
}

object MatcherResponse {
  def toResponseMarshaller(implicit context: ErrorFormatterContext): ToResponseMarshaller[MatcherResponse] = Marshaller.opaque { x =>
    def content(x: MatcherResponse): JsValue =
      backwardCompatibleWrapper(
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

  private def backwardCompatibleWrapper(code: StatusCode, json: JsObject): JsValue =
    Json
      .obj(
        "success" -> (code match {
          case _: C.Success => true
          case _            => false
        }),
        "status" -> getClass.getSimpleName.replace("$", "")
      )
      .deepMerge(code match {
        case _: C.Success => JsObject.empty
        case _            => Json.obj("result" -> JsNull) // For backward compatibility
      })
      .deepMerge(json)
}

sealed trait MatcherResponseContent
object MatcherResponseContent {
  case class Js(content: JsObject)                    extends MatcherResponseContent
  case class Error(content: MatcherError)             extends MatcherResponseContent
  case class Multiple(content: List[MatcherResponse]) extends MatcherResponseContent
}

// TODO: both classes to MatcherResponse.apply? or MatcherResponse.this(...)
sealed abstract class MatcherErrorResponse(code: StatusCode, error: MatcherError) extends MatcherResponse(code) {
  override val content = MatcherResponseContent.Error(error)
}

sealed abstract class WrappedMatcherResponse(code: StatusCode, val json: JsObject) extends MatcherResponse(code) {
  override val content = MatcherResponseContent.Js(json)
}

case class SimpleResponse(code: StatusCode, message: String) extends WrappedMatcherResponse(code, Json.obj("message"       -> message))
case object AlreadyProcessed                                 extends WrappedMatcherResponse(C.Accepted, Json.obj("message" -> "This event has been already processed"))
case class OrderAccepted(order: Order)                       extends WrappedMatcherResponse(C.OK, Json.obj("message"       -> order.json()))
case class OrderCanceled(orderId: ByteStr)                   extends WrappedMatcherResponse(C.OK, Json.obj("orderId"       -> orderId))
case class OrderDeleted(orderId: ByteStr)                    extends WrappedMatcherResponse(C.OK, Json.obj("orderId"       -> orderId))

case class BatchCancelCompleted(orders: Map[Order.Id, MatcherResponse]) extends MatcherResponse(C.OK) {
  override def content = MatcherResponseContent.Multiple(orders.values.toList)
}

case class OrderRejected(error: MatcherError)        extends MatcherErrorResponse(C.BadRequest, error)
case class OrderCancelRejected(error: MatcherError)  extends MatcherErrorResponse(C.BadRequest, error)
case object CancelRequestInvalidSignature            extends MatcherErrorResponse(C.BadRequest, error.RequestInvalidSignature)
case class NotImplemented(error: MatcherError)       extends MatcherErrorResponse(C.NotImplemented, error)
case class OrderBookUnavailable(error: MatcherError) extends MatcherErrorResponse(C.ServiceUnavailable, error)
case object DuringStart                              extends MatcherErrorResponse(C.ServiceUnavailable, error.MatcherIsStarting)
case object DuringShutdown                           extends MatcherErrorResponse(C.ServiceUnavailable, error.MatcherIsStopping)
case class InfoNotFound(error: MatcherError)         extends MatcherErrorResponse(C.NotFound, error)
