package com.wavesplatform.it

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{Format, JsError, JsString, JsSuccess, Json, Reads, Writes}

import scala.util.{Failure, Success}

package object json {
  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]
}
