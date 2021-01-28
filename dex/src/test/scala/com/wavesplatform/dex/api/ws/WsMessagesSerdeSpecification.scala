package com.wavesplatform.dex.api.ws

import com.softwaremill.diffx.Diff
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.api.http.PlayJsonException
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsLastTrade, WsOrder, WsOrderBookSettings}
import com.wavesplatform.dex.api.ws.protocol.WsOrderBookChanges.WsSide
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsOrderBookChanges, WsRatesUpdates}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder, OrderBook}
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{Format, Json}

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import scala.collection.immutable.TreeMap

class WsMessagesSerdeSpecification extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks with Matchers with MatcherSpecBase {

  implicit private val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

  private val wsBalancesGen = for {
    tradable <- maxWavesAmountGen
    reserved <- maxWavesAmountGen
    div <- Gen.choose(1, 100000000L)
  } yield WsBalances(tradable.toDouble / div, reserved.toDouble / div)

  private val wsOrderGen = for {
    (order, _) <- orderGenerator
    isMarket <- Gen.oneOf(true, false)
    isNew <- Gen.oneOf(true, false)
    filledPercent <- Gen.choose(0d, 1d)
  } yield {

    lazy val partialAmount: Long = (order.amount * filledPercent).toLong
    lazy val partialFee: Long = (order.matcherFee * filledPercent).toLong

    val ao = (isNew, isMarket) match {
      case (true, true) => MarketOrder(order, _ => Long.MaxValue)
      case (true, false) => LimitOrder(order)
      case (false, true) =>
        MarketOrder(order, _ => Long.MaxValue).partial(partialAmount, partialFee, Long.MaxValue, BigInteger.valueOf(order.price))
      case (false, false) => LimitOrder(order).partial(partialAmount, partialFee, BigInteger.valueOf(order.price))
    }

    val result = WsOrder.fromDomain(ao)

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

  private val wsAddressChangesGen = for {
    account <- Gen.alphaNumStr.map(x => KeyPair(ByteStr(x.getBytes(StandardCharsets.UTF_8))))
    balanceChanges <- Gen.choose(0, 5)
    orderChanges <- Gen.const(5 - balanceChanges)
    assets <- Gen.listOfN(balanceChanges, assetGen)
    balances <- Gen.listOfN(balanceChanges, wsBalancesGen)
    orders <- Gen.listOfN(orderChanges, wsOrderGen)
    updateId <- Gen.choose(0L, Long.MaxValue)
    ts <- Gen.choose(0L, Long.MaxValue)
  } yield WsAddressChanges(account.toAddress, (assets zip balances).toMap, orders, updateId, ts)

  private val askPricesMin = 1000L * Order.PriceConstant
  private val askPricesMax = 2000L * Order.PriceConstant
  private val askPricesGen = Gen.choose(askPricesMin, askPricesMax)

  private val bidPricesMin = 1L * Order.PriceConstant
  private val bidPricesMax = 999L * Order.PriceConstant
  private val bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)

  private val amountGen = Gen.choose(1L, 2000L)

  private val amountDecimals = 8
  private val priceDecimals = 2

  private val orderBookSettingsGen: Gen[WsOrderBookSettings] = {

    def getDenormalizedValueInRange(min: Long, max: Long): Gen[Double] =
      Gen.choose(min, max).map(BigDecimal(_) / Order.PriceConstant toDouble)

    val restrictionsGen =
      for {
        stepAmount <- getDenormalizedValueInRange(1, 10)
        minAmount <- getDenormalizedValueInRange(1, 10)
        maxAmount <- getDenormalizedValueInRange(1 * Order.PriceConstant, 10 * Order.PriceConstant)
        stepPrice <- getDenormalizedValueInRange(1, 10)
        minPrice <- getDenormalizedValueInRange(1, 10)
        maxPrice <- getDenormalizedValueInRange(1 * Order.PriceConstant, 10 * Order.PriceConstant)
      } yield OrderRestrictionsSettings(stepAmount, minAmount, maxAmount, stepPrice, minPrice, maxPrice)

    val tickSizeGen = getDenormalizedValueInRange(10, 50)

    for {
      restrictions <- Gen.option(restrictionsGen)
      tickSize <- Gen.option(tickSizeGen)
    } yield WsOrderBookSettings(restrictions, tickSize)
  }

  private val lastTradeGen: Gen[WsLastTrade] =
    for {
      price <- Gen.chooseNum(1, Long.MaxValue)
      amount <- Gen.chooseNum(1, Long.MaxValue)
      orderType <- orderTypeGenerator
    } yield WsLastTrade(
      price = Denormalization.denormalizePrice(price, 8, 2).toDouble,
      amount = Denormalization.denormalizeAmountAndFee(amount, 8).toDouble,
      orderType
    )

  private val wsOrderBookChangesGen: Gen[WsOrderBookChanges] = for {
    assetPair <- assetPairGen
    asks <- wsSide(askPricesGen, OrderBook.asksDenormalizedOrdering)
    bids <- wsSide(bidPricesGen, OrderBook.bidsDenormalizedOrdering)
    lastTrade <- Gen.oneOf[Option[WsLastTrade]](None, lastTradeGen.map(Option(_)))
    updateId <- Gen.choose(0L, Long.MaxValue)
    ts <- Gen.choose(0L, Long.MaxValue)
    orderBookSettings <- Gen.option(orderBookSettingsGen)
  } yield protocol.WsOrderBookChanges(assetPair, asks, bids, lastTrade, updateId, orderBookSettings, ts)

  private def wsSide(pricesGen: Gen[Long], ordering: Ordering[Double]): Gen[WsSide] = {
    val itemGen = Gen.zip(pricesGen, amountGen)
    Gen.listOf(itemGen).map { xs =>
      TreeMap(xs.map {
        case (price, amount) =>
          Denormalization.denormalizePrice(price, amountDecimals, priceDecimals).toDouble ->
            Denormalization.denormalizeAmountAndFee(amount, amountDecimals).toDouble
      }: _*)(ordering)
    }
  }

  private val wsRatesUpdateGen: Gen[WsRatesUpdates] = for {
    timestamp <- Gen.choose(0L, Long.MaxValue)
    updateId <- Gen.choose(0L, Long.MaxValue)
    ratesCount <- Gen.choose(1, 5)
    assets <- Gen.listOfN(ratesCount, assetGen)
    rates <- Gen.listOfN(ratesCount, Gen.frequency((4, Gen.choose(0.05d, 100500d)), (1, Gen.const(-1d))))
  } yield WsRatesUpdates(assets.zip(rates).toMap, updateId, timestamp)

  private def serdeTest[T <: Product with Serializable: Diff](gen: Gen[T])(implicit format: Format[T]): Unit = forAll(gen) { original =>
    val json = format writes original
    withClue(s"${Json.stringify(json)}: ") {
      val restored =
        format
          .reads(json)
          .fold(
            e => throw PlayJsonException(None, e.map { case (jp, errorsSeq) => jp -> errorsSeq.to(Seq) } to Seq),
            identity
          )

      original should matchTo(restored)
    }
  }

  "WsOrder" in serdeTest(wsOrderGen)

  "WsAddressChanges" in serdeTest(wsAddressChangesGen)

  "WsOrderBookChanges" in serdeTest(wsOrderBookChangesGen)

  "WsRatesUpdates" in serdeTest(wsRatesUpdateGen)
}
