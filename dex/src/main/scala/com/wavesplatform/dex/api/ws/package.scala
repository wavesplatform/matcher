package com.wavesplatform.dex.api

import play.api.libs.json._

package object ws {

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
}
