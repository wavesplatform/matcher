package com.wavesplatform.dex.domain.serialization

import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.JsObject

trait ByteAndJsonSerializable {

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]]

  val json: Coeval[JsObject]
}
