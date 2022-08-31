package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpAddressCheck(
  @ApiModelProperty matcher: Boolean,
  @ApiModelProperty blockchain: Boolean
)

object HttpAddressCheck {

  implicit val HttpAddressCheckFormat: Format[HttpAddressCheck] = Json.format
}
