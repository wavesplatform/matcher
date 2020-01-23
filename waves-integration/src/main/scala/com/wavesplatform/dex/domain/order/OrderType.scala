package com.wavesplatform.dex.domain.order

import play.api.libs.json._

sealed trait OrderType extends Product with Serializable {
  def bytes: Array[Byte]
  def opposite: OrderType
}

object OrderType {

  case object BUY extends OrderType {
    def bytes: Array[Byte]           = Array(0.toByte)
    override def opposite: OrderType = SELL
    override val toString: String    = "buy"
  }

  case object SELL extends OrderType {
    def bytes: Array[Byte]           = Array(1.toByte)
    override def opposite: OrderType = BUY
    override val toString: String    = "sell"
  }

  def apply(value: Int): OrderType = value match {
    case 0 => OrderType.BUY
    case 1 => OrderType.SELL
    case _ => throw new RuntimeException(s"Unexpected OrderType: $value")
  }

  def reverse(orderType: OrderType): OrderType = orderType match {
    case BUY  => SELL
    case SELL => BUY
  }

  implicit val orderTypeFormat: Format[OrderType] = Format(
    Reads {
      case JsString(x) =>
        x match {
          case "buy"  => JsSuccess(BUY)
          case "sell" => JsSuccess(SELL)
          case _      => JsError(s"Unknown order type: $x")
        }
      case x => JsError(s"Can't parse '$x' as OrderType")
    },
    Writes {
      case BUY  => JsString("buy")
      case SELL => JsString("sell")
    }
  )
}
