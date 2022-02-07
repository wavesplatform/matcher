package com.wavesplatform.dex.codecs

import cats.syntax.option._
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.codecs.ByteBufferCodecs.ByteBufferExt
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{BuyLimitOrder, LimitOrder, OrderBookSideSnapshot, SellLimitOrder}

import java.math.BigInteger
import java.nio.ByteBuffer
import scala.collection.mutable

object OrderBookSideSnapshotCodecs {

  def encode(dest: mutable.ArrayBuilder[Byte], snapshot: OrderBookSideSnapshot): Unit = {
    dest ++= Ints.toByteArray(snapshot.size)
    snapshot.foreach {
      case (price, xs) =>
        dest ++= Longs.toByteArray(price)
        dest ++= Ints.toByteArray(xs.size)
        xs.foreach(encodeLoV3(dest, _))
    }
  }

  def decode(bb: ByteBuffer): OrderBookSideSnapshot = {
    val snapshotSize = bb.getInt
    val r = Map.newBuilder[Price, Seq[LimitOrder]]
    (1 to snapshotSize).foreach { _ =>
      val price = bb.getLong
      val levelSize = bb.getInt
      val limitOrders = (1 to levelSize).map(_ => decodeLo(bb))
      r += price -> limitOrders
    }
    r.result()
  }

  def encodeLoV3(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {
    dest += 3 //version

    dest ++= lo.order.orderType.bytes
    dest ++= Longs.toByteArray(lo.amount)
    dest ++= Longs.toByteArray(lo.fee)
    dest += lo.order.version

    val orderBytes = lo.order.bytes()

    dest ++= Ints.toByteArray(orderBytes.length)
    dest ++= orderBytes

    val avgWeighedPriceNominatorBytes = lo.avgWeighedPriceNominator.toByteArray
    dest ++= Ints.toByteArray(avgWeighedPriceNominatorBytes.length)
    dest ++= avgWeighedPriceNominatorBytes

    val (mfLen, mf) = lo.percentMinFee.map { mf =>
      val mfBytes = Longs.toByteArray(mf)
      Ints.toByteArray(mfBytes.length) -> mfBytes
    }.getOrElse(Ints.toByteArray(0) -> Array.emptyByteArray)
    dest ++= mfLen
    dest ++= mf

    val (cmfLen, cmf) = lo.percentConstMinFee.map { cmf =>
      val cmfBytes = Longs.toByteArray(cmf)
      Ints.toByteArray(cmfBytes.length) -> cmfBytes
    }.getOrElse(Ints.toByteArray(0) -> Array.emptyByteArray)
    dest ++= cmfLen
    dest ++= cmf
  }

  def decodeLo(bb: ByteBuffer): LimitOrder = {
    //!!! pay attention that v1 order doesn't have first byte with version
    val header = bb.get
    val version = if (header == 0 || header == 1) 1 else header.toInt
    val orderType = if (version == 1) header else bb.get

    val amount = bb.getLong
    val fee = bb.getLong
    val orderVersion = bb.get
    val order = Order.fromBytes(orderVersion, bb.getBytes)._1

    val avgWeighedPriceNominator =
      if (version == 1) {
        val filledAmount = order.amount - amount
        (BigInt(order.price) * filledAmount).bigInteger
      } else
        new BigInteger(bb.getBytes)

    val (percentMinFee, percentConstMinFee) =
      if (version >= 3) {
        val mfBytes = bb.getBytes
        val mf =
          if (mfBytes.nonEmpty) Longs.fromByteArray(mfBytes).some else None
        val cmfBytes = bb.getBytes
        val cmf =
          if (cmfBytes.nonEmpty) Longs.fromByteArray(cmfBytes).some else None
        mf -> cmf
      } else
        None -> None

    OrderType(orderType) match {
      case OrderType.SELL => SellLimitOrder(amount, fee, order, avgWeighedPriceNominator, percentMinFee, percentConstMinFee)
      case OrderType.BUY => BuyLimitOrder(amount, fee, order, avgWeighedPriceNominator, percentMinFee, percentConstMinFee)
    }
  }

}
