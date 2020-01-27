package com.wavesplatform.dex

import com.wavesplatform.dex.model.OrderBookResult
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}

import scala.util.control.NonFatal

package object api {
  // Here, because it is not a production ready and should be used only in tests
  implicit val orderBookResultReads: Reads[OrderBookResult] = Reads { json =>
    try JsSuccess(JsonSerializer.deserialize[OrderBookResult](Json.stringify(json)))
    catch {
      case NonFatal(e) => JsError(s"Can't deserialize OrderBookResult: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
    }
  }
}
