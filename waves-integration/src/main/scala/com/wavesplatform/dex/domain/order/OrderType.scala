package com.wavesplatform.dex.domain.order

import play.api.libs.json.{Format, JsError, JsString, JsSuccess}

sealed trait OrderType {
  def bytes: Array[Byte]
  def opposite: OrderType
}

object OrderType {

  case object BUY extends OrderType {
    def bytes: Array[Byte]           = Array(0.toByte)
    override def opposite: OrderType = SELL
    override def toString: String    = "buy"
  }

  case object SELL extends OrderType {
    def bytes: Array[Byte]           = Array(1.toByte)
    override def opposite: OrderType = BUY
    override def toString: String    = "sell"
  }

  def apply(value: Int): OrderType = value match {
    case 0 => OrderType.BUY
    case 1 => OrderType.SELL
    case _ => throw new RuntimeException(s"Unexpected OrderType: $value")
  }

  def apply(value: String): OrderType = value match {
    case "buy"  => OrderType.BUY
    case "sell" => OrderType.SELL
    case _      => throw new RuntimeException("Unexpected OrderType")
  }

  def reverse(orderType: OrderType): OrderType = orderType match {
    case BUY  => SELL
    case SELL => BUY
  }

  implicit val format: Format[OrderType] = Format(
    {
      case JsString("BUY")  => JsSuccess(BUY)
      case JsString("SELL") => JsSuccess(SELL)
      case _                => JsError("Cannot parse order type!")
    }, {
      case BUY  => JsString("BUY")
      case SELL => JsString("SELL")
    }
  )
}
