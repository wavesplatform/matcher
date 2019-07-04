package com.wavesplatform.dex

import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._

package object error {
  implicit class ErrorInterpolator(private val sc: StringContext) {
    def e(args: (Symbol, Any)*): MatcherErrorMessage = {
      val parts = sc.parts.init

      val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
        case ((m, t, p), (x, i)) =>
          val (argName, argValue) = args(i)
          val strValue            = Option(argValue).map(toString).getOrElse("<null>")
          val jsonValue           = Option(argValue).map(toJson).getOrElse(JsNull)
          (s"$m$x$strValue", s"$t$x{{${argName.name}}}", p + (argName.name -> jsonValue))
      }

      MatcherErrorMessage(
        (message + sc.parts.last).trim,
        (template + sc.parts.last).trim,
        params
      )
    }

    private def toString(o: Any): String = o match {
      case xs: Map[_, _] =>
        val map = xs.map {
          case (rawK, v) =>
            val k = rawK match {
              case x: Asset => AssetPair.assetIdStr(x)
              case _        => rawK.toString
            }
            s"$k: $v"
        }
        s"""{${map.mkString(", ")}}"""

      case x => x.toString
    }

    private def toJson(o: Any): JsValue = o match {
      case x: String     => JsString(x)
      case x: Byte       => JsNumber(BigDecimal(x))
      case x: Int        => JsNumber(BigDecimal(x))
      case x: Long       => JsNumber(BigDecimal(x))
      case x: Double     => JsNumber(BigDecimal(x))
      case x: BigDecimal => JsNumber(x)
      case x: Float      => toJson(x.toDouble)
      case x: Boolean    => JsBoolean(x)
      case xs: Map[_, _] =>
        JsObject(xs.map {
          case (rawK, v) =>
            val k = rawK match {
              case x: Asset => AssetPair.assetIdStr(x)
              case _        => rawK.toString
            }

            k -> toJson(v)
        })

      case x => JsString(x.toString)
    }
  }
}
