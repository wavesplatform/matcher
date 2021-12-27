package com.wavesplatform.dex.domain.order

import play.api.libs.json._

sealed trait OrderType extends Product with Serializable {
  def bytes: Array[Byte]
  def opposite: OrderType
}

object OrderType {

  case object BUY extends OrderType {
    def bytes: Array[Byte] = Array(0.toByte)
    override def opposite: OrderType = SELL
    override val toString: String = "buy"
  }

  case object SELL extends OrderType {
    def bytes: Array[Byte] = Array(1.toByte)
    override def opposite: OrderType = BUY
    override val toString: String = "sell"
  }

  def apply(value: Int): OrderType = value match {
    case 0 => OrderType.BUY
    case 1 => OrderType.SELL
    case _ => throw new RuntimeException(s"Unexpected OrderType: $value")
  }

  def reverse(orderType: OrderType): OrderType = orderType match {
    case BUY => SELL
    case SELL => BUY
  }

  implicit val orderTypeFormat: Format[OrderType] = Format(
    {
      case JsString(BUY.`toString`) => JsSuccess(BUY)
      case JsString(SELL.`toString`) => JsSuccess(SELL)
      case x => JsError(JsPath, s"Can't read OrderType from ${x.getClass.getName}")
    },
    {
      case BUY => JsString(BUY.`toString`)
      case SELL => JsString(SELL.`toString`)
    }
  )

  implicit final class OrderTypeOps(val self: OrderType) extends AnyVal {
    def askBid[T](ifAsk: => T, ifBid: => T): T = if (self == OrderType.SELL) ifAsk else ifBid
    def opposite: OrderType = askBid(OrderType.BUY, OrderType.SELL)
  }

}
