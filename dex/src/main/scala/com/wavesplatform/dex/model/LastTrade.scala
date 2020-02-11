package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.order.OrderType
import play.api.libs.json._

import scala.collection.mutable
import scala.util.Try

case class LastTrade(price: Long, amount: Long, side: OrderType)
object LastTrade {
  // Only for serialization
  implicit val format: Format[LastTrade] = {
    implicit val orderTypeFormat: Format[OrderType] = Format(
      {
        case JsNumber(x) => Try(OrderType(x.toIntExact)).fold(e => JsError(s"Can't deserialize $x as OrderType: ${e.getMessage}"), JsSuccess(_))
        case x           => JsError(s"Can't deserialize $x as OrderType")
      },
      x => JsNumber(x.bytes.head.toInt)
    )

    Json.format[LastTrade]
  }

  def serialize(dest: mutable.ArrayBuilder[Byte], x: LastTrade): Unit = {
    dest ++= Longs.toByteArray(x.price)
    dest ++= Longs.toByteArray(x.amount)
    dest ++= x.side.bytes
  }

  def fromBytes(bb: ByteBuffer): LastTrade = LastTrade(bb.getLong, bb.getLong, OrderType(bb.get))
}
