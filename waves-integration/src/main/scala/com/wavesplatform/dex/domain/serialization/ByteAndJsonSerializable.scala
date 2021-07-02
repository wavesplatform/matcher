package com.wavesplatform.dex.domain.serialization

import monix.eval.Coeval
import play.api.libs.json.JsObject

trait ByteAndJsonSerializable {
  val bytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]
}
