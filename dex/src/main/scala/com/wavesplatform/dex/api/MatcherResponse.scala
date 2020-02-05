package com.wavesplatform.dex.api

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCodes => C, _}
import akka.util.ByteString
import com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.util.getSimpleName
import play.api.libs.json._

sealed class MatcherResponse(val statusCode: StatusCode, val content: MatcherResponseContent) {

  def this(code: StatusCode, error: MatcherError) = this(code, MatcherResponseContent.Single(Json.toJsObject(error)))
  def this(code: StatusCode, error: JsObject) = this(code, MatcherResponseContent.Single(error))

  def status: String = getSimpleName(this)
}

object MatcherResponse {

  def toResponseMarshaller: ToResponseMarshaller[MatcherResponse] = Marshaller.opaque { x =>
    def content(x: MatcherResponse): JsValue = backwardCompatibleWrapper(
      x.status,
      x.statusCode,
      x.content match {
        case MatcherResponseContent.Single(r)    => r
        case MatcherResponseContent.Multiple(xs) => Json.obj("message" -> Json.arr(xs.map(content)))
      }
    )

    HttpResponse(
      x.statusCode,
      entity = HttpEntity.Strict(ContentTypes.`application/json`, ByteString(Json.stringify(content(x))))
    )
  }

  private def backwardCompatibleWrapper(status: String, code: StatusCode, json: JsObject): JsValue = {
    Json
      .obj(
        "success" -> (
          code match {
            case _: C.Success => true
            case _            => false
          }
        ),
        "status" -> status
      )
      .deepMerge(
        code match {
          case _: C.Success => JsObject.empty
          case _            => Json.obj("result" -> JsNull) // For a backward compatibility
        }
      )
      .deepMerge(json)
  }
}

sealed trait MatcherResponseContent
object MatcherResponseContent {
  case class Single(content: JsObject)                extends MatcherResponseContent
  case class Multiple(content: List[MatcherResponse]) extends MatcherResponseContent
}

case class SimpleResponse(code: StatusCode, js: JsObject) extends MatcherResponse(code, MatcherResponseContent.Single(js))

object SimpleResponse {
  def apply(code: StatusCode, message: String): SimpleResponse = new SimpleResponse(code, Json.obj("message" -> message))
}

case class OrderCanceled(orderId: Order.Id) extends MatcherResponse(C.OK, Json.obj("orderId" -> orderId))
case class OrderDeleted(orderId: Order.Id)  extends MatcherResponse(C.OK, Json.obj("orderId" -> orderId))

case class SimpleErrorResponse(code: StatusCode, error: MatcherError)      extends MatcherResponse(code, error)
case class InvalidJsonResponse(error: MatcherError)                        extends MatcherResponse(C.BadRequest, error)
case class OrderRejected(error: MatcherError)                              extends MatcherResponse(C.BadRequest, error)
case class OrderCancelRejected(error: MatcherError)                        extends MatcherResponse(C.BadRequest, error)
case object InvalidSignature                                               extends MatcherResponse(C.BadRequest, error.RequestInvalidSignature)
case class NotImplemented(error: MatcherError)                             extends MatcherResponse(C.NotImplemented, error)
case class CanNotPersist(error: MatcherError)                              extends MatcherResponse(C.ServiceUnavailable, error)
case class OrderBookUnavailable(error: MatcherError)                       extends MatcherResponse(C.ServiceUnavailable, error)
case object DuringStart                                                    extends MatcherResponse(C.ServiceUnavailable, error.MatcherIsStarting)
case object DuringShutdown                                                 extends MatcherResponse(C.ServiceUnavailable, error.MatcherIsStopping)
case object TimedOut                                                       extends MatcherResponse(C.InternalServerError, error.RequestTimeout)
case class InfoNotFound(error: MatcherError)                               extends MatcherResponse(C.NotFound, error)
case class WavesNodeUnavailable(error: MatcherError)                       extends MatcherResponse(C.ServiceUnavailable, error)
case class RateError(error: MatcherError, code: StatusCode = C.BadRequest) extends MatcherResponse(code, error)
case object InternalError                                                  extends MatcherResponse(C.ServiceUnavailable, MatcherResponseContent.Single(Json.obj("message" -> "Internal server error")))
