package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.error.MatcherError
import play.api.libs.json.{Format, JsObject, Json}

case class HttpError(error: Int,
                     message: String,
                     template: String,
                     params: Option[JsObject] = None,
                     status: String, // @deprecated(message = "This field is unnecessary", since = "1.2.0")
                     success: Boolean = false,
)
object HttpError {

  implicit val httpErrorFormat: Format[HttpError] = Json.format

  def apply(error: Int, message: String, template: String, params: JsObject, status: String): HttpError = new HttpError(
    error = error,
    message = message,
    template = template,
    params = Some(params),
    status = status
  )

  def from(x: MatcherError, status: String): HttpError = HttpError(
    error = x.code,
    message = x.message.text,
    template = x.message.template,
    params = if (x.message.params == JsObject.empty) None else Some(x.message.params),
    status = status
  )
}
