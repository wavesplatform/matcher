package com.wavesplatform.dex.model

import java.math.BigInteger

import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.OrderInfoSpec.OrderExt
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderInfoSpec extends AnyFreeSpec with Matchers with MatcherSpecBase with PropertyChecks with NoShrink {

  private def finalizedOrderInfoGen(o: Order, orderInfoVersion: Byte): Gen[OrderInfo[OrderStatus.Final]] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      filledFee <- if (orderInfoVersion == 1) Gen.const((BigInt(filledAmount) * 300000 / o.amount).toLong) else Gen.choose(0, o.matcherFee)
      status <- Gen.oneOf(OrderStatus.Filled(filledAmount, filledFee), OrderStatus.Cancelled(filledAmount, filledFee))
      aoType <- if (orderInfoVersion <= 2) Gen.const(AcceptedOrderType.Limit) else Gen.oneOf(AcceptedOrderType.Limit, AcceptedOrderType.Market)
      avgWeighedPrice <- if (orderInfoVersion <= 3) Gen.const(o.price) else Gen.choose(0, o.price)
    } yield o.toInfo(orderInfoVersion, status, aoType, avgWeighedPrice, OrderInfo.getAvgWeighedPriceNominator(filledAmount, avgWeighedPrice))

  private val finalizedOrderInfoGen: Gen[OrderInfo[OrderStatus.Final]] = for {
    (o, _) <- orderGenerator
    v <- Gen.choose[Byte](1, 4)
    result <- finalizedOrderInfoGen(o, v)
  } yield result

  "OrderInfo" - {
    "x == decode(encode(x))" in forAll(finalizedOrderInfoGen) { oi =>
      OrderInfo.decode(OrderInfo encode oi) == oi
    }
  }
}

object OrderInfoSpec {

  implicit class OrderExt(val o: Order) extends AnyVal {

    def toInfo[A <: OrderStatus](
      version: Byte,
      status: A,
      aoType: AcceptedOrderType,
      avgWeighedPrice: Long,
      avgWeighedPriceNominator: BigInteger
    ): OrderInfo[A] = version match {
      case 1 => OrderInfo.v1[A](o.orderType, o.amount, o.price, o.timestamp, status, o.assetPair)
      case 2 => OrderInfo.v2[A](o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, status, o.assetPair)
      case 3 => OrderInfo.v3[A](o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, status, o.assetPair, aoType)
      case 4 =>
        OrderInfo.v4[A](o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, status, o.assetPair, aoType, avgWeighedPrice)
      case 5 =>
        OrderInfo
          .v5[A](o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, status, o.assetPair, aoType, avgWeighedPrice, o.version)
      case 6 =>
        OrderInfo
          .v6[A](
            o.orderType,
            o.amount,
            o.price,
            o.matcherFee,
            o.feeAsset,
            o.timestamp,
            status,
            o.assetPair,
            aoType,
            o.version,
            avgWeighedPriceNominator
          )
    }

  }

}
