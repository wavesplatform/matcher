package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.model.{BuyLimitOrder, LastTrade, OrderBookSideSnapshot, OrderBookSnapshot, SellLimitOrder}
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderBookSnapshotDbSpec extends AnyFreeSpec with Matchers with WithDb with MatcherSpecBase with PropertyChecks with NoShrink {

  private val fixedAssetPairGen = assetPairGen.filterNot(x => x.amountAsset == Waves && x.priceAsset == Waves)

  "Default OrderBookSnapshotDb implementation" - {
    "stores and reads all asset pairs" in {
      forAll(gen) { case (snapshot, assetPair, offset) =>
        test { obsdb =>
          obsdb.update(assetPair, offset, Some(snapshot))
          obsdb.get(assetPair) shouldBe Some(offset -> snapshot)
        }
      }
    }

    "remove asset pair" in {
      forAll(gen) { case (snapshot, assetPair, offset) =>
        test { obsdb =>
          obsdb.update(assetPair, offset, Some(snapshot))
          obsdb.delete(assetPair)
          obsdb.get(assetPair) shouldBe empty
        }
      }
    }
  }

  private def test(f: OrderBookSnapshotDb[Id] => Any): Any = tempLevelDb(db => f(OrderBookSnapshotDb.levelDb(db)))

  private val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  private val asksGen: Gen[OrderBookSideSnapshot] = for {
    n <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[SellLimitOrder]](n, sellLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  private val bidsGen: Gen[OrderBookSideSnapshot] = for {
    n <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[BuyLimitOrder]](n, buyLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private lazy val lastTradeGen: Gen[LastTrade] = for {
    price <- Gen.choose(1, Long.MaxValue)
    amount <- Gen.choose(1, Long.MaxValue)
    orderType <- orderTypeGenerator
  } yield LastTrade(price, amount, orderType)

  private lazy val snapshotGen: Gen[OrderBookSnapshot] = for {
    asks <- asksGen
    bids <- bidsGen
    lastTrade <- Gen.option(lastTradeGen)
  } yield OrderBookSnapshot(bids, asks, lastTrade)

  private lazy val gen = for {
    snapshot <- snapshotGen
    assetPair <- fixedAssetPairGen
    offset <- Gen.choose(0L, Long.MaxValue)
  } yield (snapshot, assetPair, offset)

}
