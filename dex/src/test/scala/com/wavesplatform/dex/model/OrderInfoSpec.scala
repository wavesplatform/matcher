package com.wavesplatform.dex.model

import com.wavesplatform.NoShrink
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.dex.model.OrderInfoSpec.OrderExt
import com.wavesplatform.transaction.assets.exchange.Order
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderInfoSpec extends FreeSpec with Matchers with MatcherTestData with PropertyChecks with NoShrink {
  private def finalizedOrderInfoGen(o: Order, orderInfoVersion: Byte): Gen[OrderInfo[OrderStatus.Final]] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      filledFee    <- if (orderInfoVersion == 1) Gen.const((BigInt(filledAmount) * 300000 / o.amount).toLong) else Gen.choose(0, o.matcherFee)
      status       <- Gen.oneOf(OrderStatus.Filled(filledAmount, filledFee), OrderStatus.Cancelled(filledAmount, filledFee))
    } yield o.toInfo(orderInfoVersion, status)

  private val finalizedOrderInfoGen: Gen[OrderInfo[OrderStatus.Final]] = for {
    (o, _) <- orderGenerator
    v      <- Gen.choose[Byte](1, 2)
    result <- finalizedOrderInfoGen(o, v)
  } yield result

  "OrderInfo" - {
    "x == decode(encode(x))" in forAll(finalizedOrderInfoGen) { oi =>
      OrderInfo.decode(OrderInfo.encode(oi)) == oi
    }
  }
}

object OrderInfoSpec {
  implicit class OrderExt(val o: Order) extends AnyVal {
    def toInfo[A <: OrderStatus](version: Byte, status: A) = version match {
      case 1 => OrderInfo.v1[A](o.orderType, o.amount, o.price, o.timestamp, status, o.assetPair)
      case 2 => OrderInfo.v2[A](o.orderType, o.amount, o.price, o.matcherFee, o.matcherFeeAssetId, o.timestamp, status, o.assetPair)
    }
  }
}
