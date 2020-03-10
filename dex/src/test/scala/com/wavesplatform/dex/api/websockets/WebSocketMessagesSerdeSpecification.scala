package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.model.{LimitOrder, MarketOrder}
import com.wavesplatform.dex.{AddressActor, MatcherSpecBase}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Format

class WebSocketMessagesSerdeSpecification extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks with Matchers with MatcherSpecBase {

  private val wsBalancesGen = for {
    tradable <- maxWavesAmountGen
    reserved <- maxWavesAmountGen
  } yield WsBalances(tradable, reserved)

  private val wsOrderGen = for {
    (order, _)    <- orderGenerator
    isMarket      <- Gen.oneOf(true, false)
    isNew         <- Gen.oneOf(true, false)
    filledPercent <- Gen.choose(0D, 1D)
  } yield {

    lazy val partialAmount: Long = (order.amount * filledPercent).toLong
    lazy val partialFee: Long    = (order.matcherFee * filledPercent).toLong

    val ao = (isNew, isMarket) match {
      case (true, true)   => MarketOrder(order, _ => Long.MaxValue)
      case (true, false)  => LimitOrder(order)
      case (false, true)  => MarketOrder(order, _ => Long.MaxValue).partial(partialAmount, partialFee, Long.MaxValue)
      case (false, false) => LimitOrder(order).partial(partialAmount, partialFee)
    }

    val result = WsOrder.fromDomain(ao, AddressActor.activeStatus(ao))

    if (isNew) result
    else
      result.copy(
        timestamp = None, // simulate partly filling of an already tracked order
        amountAsset = None,
        priceAsset = None,
        side = None,
        isMarket = None,
        price = None,
        amount = None,
        fee = None,
        feeAsset = None
      )
  }

  private val wsAddressStateGen = for {
    balanceChanges <- Gen.choose(0, 5)
    orderChanges   <- Gen.const(5 - balanceChanges)
    assets         <- Gen.listOfN(balanceChanges, assetGen)
    balances       <- Gen.listOfN(balanceChanges, wsBalancesGen)
    orders         <- Gen.listOfN(orderChanges, wsOrderGen)
  } yield WsAddressState((assets zip balances).toMap, orders)

  private def serdeTest[T](gen: Gen[T])(implicit format: Format[T]): Unit = forAll(gen) { x =>
    x shouldBe format.reads(format writes x).get
  }

  "WsOrder" in serdeTest(wsOrderGen)

  "WsAddressState" in serdeTest(wsAddressStateGen)
}
