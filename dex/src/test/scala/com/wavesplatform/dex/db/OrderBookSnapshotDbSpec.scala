package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
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

    "iterate over snapshots & offsets (1)" in {
      forAll(genSeq) { v =>
        test { obsdb =>
          v.foreach { case (snapshot, assetPair, offset) =>
            obsdb.update(assetPair, offset, Some(snapshot))
          }
          val snapshots = obsdb.iterateSnapshots(_ => true)
          val offsets = obsdb.iterateOffsets(_ => true)

          val expected = v.map(_._2).toSet.map { pair: AssetPair =>
            pair -> offsets.get(pair).zip(snapshots.get(pair))
          }.toMap

          val actual = v.map { case (snapshot, assetPair, offset) =>
            assetPair -> Some((offset, snapshot))
          }.toMap

          actual shouldBe expected
        }
      }
    }

    "iterate over snapshots & offsets (2)" in {
      val genSeqWithIgnoredPairs =
        for {
          v <- genSeq
          ignoredPairs <- Gen.pick(v.size / 2, v).map(_.map(_._2).toSet)
        } yield (v, ignoredPairs)

      forAll(genSeqWithIgnoredPairs) { case (v, ignoredPairs) =>
        test { obsdb =>
          v.foreach { case (snapshot, assetPair, offset) =>
            obsdb.update(assetPair, offset, Some(snapshot))
          }

          val filteredV = v.filterNot { case (_, pair, _) => ignoredPairs.contains(pair) }
          val snapshots = obsdb.iterateSnapshots(!ignoredPairs.contains(_))
          val offsets = obsdb.iterateOffsets(!ignoredPairs.contains(_))

          filteredV.size shouldBe snapshots.size
          filteredV.size shouldBe offsets.size

          val expected = filteredV.map(_._2).toSet.map { pair: AssetPair =>
            pair -> offsets.get(pair).zip(snapshots.get(pair))
          }.toMap

          val actual = filteredV.map { case (snapshot, assetPair, offset) =>
            assetPair -> Some((offset, snapshot))
          }.toMap

          actual shouldBe expected
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
    matchTs <- Gen.choose(0L, System.currentTimeMillis())
  } yield OrderBookSnapshot(bids, asks, lastTrade, matchTs)

  private lazy val gen = for {
    snapshot <- snapshotGen
    assetPair <- fixedAssetPairGen
    offset <- Gen.choose(0L, Long.MaxValue)
  } yield (snapshot, assetPair, offset)

  private lazy val genSeq = for {
    n <- Gen.choose(0, 10)
    v <- Gen.containerOfN[Vector, (OrderBookSnapshot, AssetPair, Long)](n, gen)
  } yield v

}
