package com.wavesplatform.dex.api

import com.wavesplatform.dex.error.MatcherError
import play.api.libs.json.{Format, JsObject, Json}

case class ApiError(
    error: Int,
    message: String,
    template: String,
    params: Option[JsObject] = None,
    @deprecated(message = "This field is unnecessary", since = "1.2.0") status: String,
    success: Boolean = false,
)
object ApiError {
  implicit val apiRejectedPlaceFormat: Format[ApiError] = Json.format

  def from(x: MatcherError, status: String): ApiError = ApiError(
    error = x.code,
    message = x.message.text,
    template = x.message.template,
    params = if (x.message.params == JsObject.empty) None else Some(x.message.params),
    status = status
  )
}
