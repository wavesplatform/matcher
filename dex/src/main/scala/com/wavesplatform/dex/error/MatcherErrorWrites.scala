package com.wavesplatform.dex.error

import com.wavesplatform.transaction.Asset
import play.api.libs.json.{JsObject, JsValue, Json}

class MatcherErrorWrites(assetDecimals: Asset => Option[Int]) {
  def writes(error: MatcherError): JsValue = {
    import error._

    val wrappedParams = if (error.message.params == JsObject.empty) JsObject.empty else Json.obj("params" -> error.message.params)
    Json
      .obj(
        "error"    -> code,
        "message"  -> error.message.text,
        "template" -> error.message.template
      )
      .deepMerge(wrappedParams)
  }
}
