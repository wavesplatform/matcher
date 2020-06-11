package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.error.MatcherError
import play.api.libs.json.{Format, JsObject, Json}

case class ApiError(
    error: Int,
    message: String,
    template: String,
    params: Option[JsObject] = None,
    status: String, // @deprecated(message = "This field is unnecessary", since = "1.2.0")
    success: Boolean = false,
)
object ApiError {

  implicit val apiErrorFormat: Format[ApiError] = Json.format

  def apply(error: Int, message: String, template: String, params: JsObject, status: String): ApiError = new ApiError(
    error = error,
    message = message,
    template = template,
    params = Some(params),
    status = status
  )

  def from(x: MatcherError, status: String): ApiError = ApiError(
    error = x.code,
    message = x.message.text,
    template = x.message.template,
    params = if (x.message.params == JsObject.empty) None else Some(x.message.params),
    status = status
  )
}
