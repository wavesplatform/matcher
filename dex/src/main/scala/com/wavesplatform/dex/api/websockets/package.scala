package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.AssetPair
import play.api.libs.json._

package object websockets {

  val doubleAsStringFormat: Format[Double] = Format(
    fjs = Reads {
      case JsString(x) =>
        try JsSuccess(x.toDouble)
        catch {
          case _: NumberFormatException => JsError(JsPath, "Invalid number")
        }

      case x => JsError(JsPath, s"Can't read Double from ${x.getClass.getName}")
    },
    tjs = Writes(x => JsString(x.toString))
  )

  val assetPairKeyAsStringFormat: Format[AssetPair] = Format(
    fjs = Reads {
      case JsString(x) => AssetPair.extractAssetPair(x).fold(e => JsError(e.getMessage), JsSuccess(_))
      case x           => JsError(JsPath, s"Expected a string, but got ${x.toString().take(10)}...")
    },
    tjs = Writes { x =>
      JsString(x.key)
    }
  )
}
