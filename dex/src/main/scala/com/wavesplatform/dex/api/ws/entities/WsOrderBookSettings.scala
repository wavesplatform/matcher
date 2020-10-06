package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws.doubleAsStringFormat
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrderBookSettings(restrictions: Option[OrderRestrictionsSettings], tickSize: Option[Double])

object WsOrderBookSettings {

  private val amountField = "a"
  private val priceField = "p"

  implicit private val doubleFormat: Format[Double] = doubleAsStringFormat

  implicit val orderRestrictionsSettingsFormat: Format[OrderRestrictionsSettings] = Format(
    fjs = Reads {
      case JsObject(restrictions) if restrictions.keySet == Set(amountField, priceField) =>
        def getRestrictions(restricted: String): (Double, Double, Double) =
          Reads.Tuple3R[Double, Double, Double].reads(restrictions(restricted)).get
        val (minAmount, stepAmount, maxAmount) = getRestrictions(amountField)
        val (minPrice, stepPrice, maxPrice) = getRestrictions(priceField)
        JsSuccess(OrderRestrictionsSettings(stepAmount, minAmount, maxAmount, stepPrice, minPrice, maxPrice))
      case x => JsError(JsPath, s"Cannot parse OrderRestrictionsSettings from ${x.getClass.getName}")
    },
    tjs = Writes { ors =>
      Json.obj(
        amountField -> Json.arr(ors.minAmount, ors.stepAmount, ors.maxAmount),
        priceField -> Json.arr(ors.minPrice, ors.stepPrice, ors.maxPrice)
      )
    }
  )

  implicit val wsOrderBookSettingsFormat: Format[WsOrderBookSettings] = (
    (__ \ "r").formatNullable[OrderRestrictionsSettings] and
      (__ \ "m" \ "t").formatNullable[Double]
  )(
    (restrictions, tickSize) => WsOrderBookSettings(restrictions, tickSize),
    unlift(WsOrderBookSettings.unapply)
  )

}
