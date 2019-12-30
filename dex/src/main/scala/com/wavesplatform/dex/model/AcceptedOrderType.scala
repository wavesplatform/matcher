package com.wavesplatform.dex.model

import play.api.libs.json._

// TODO Rename OrderType to OrderSide and AcceptedOrderType to OrderType in master after DEX-526
sealed trait AcceptedOrderType

object AcceptedOrderType {
  case object Limit extends AcceptedOrderType
  case object Market extends AcceptedOrderType

  implicit val acceptedOrderTypeFormat: Format[AcceptedOrderType] = Format(
    Reads {
      case JsString(value) =>
        value match {
          case "limit"  => JsSuccess(AcceptedOrderType.Limit)
          case "market" => JsSuccess(AcceptedOrderType.Market)
          case x        => JsError(s"Unknown order type: $x")
        }
      case x => JsError(s"Can't parse '$x' as AcceptedOrderType")
    },
    Writes {
      case Limit  => JsString("limit")
      case Market => JsString("market")
    }
  )
}
