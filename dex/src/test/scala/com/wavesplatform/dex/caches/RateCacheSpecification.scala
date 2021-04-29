package com.wavesplatform.dex.caches

import com.wavesplatform.dex.db.{RateDb, TestRateDb, WithDb}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.ExecutionContext.Implicits.global

class RateCacheSpecification extends AnyWordSpecLike with Matchers with WithDb with MatcherSpecBase with PropertyChecks with NoShrink {

  private def test(f: RateCache => Unit): Unit = {
    withClue("with DB")(f(RateCache(TestRateDb()).futureValue))
    withClue("in mem")(f(RateCache(TestRateDb()).futureValue))
  }

  private val WavesRate: Map[Asset, Double] = Map(Waves -> 1d)

  "RateCache" should {

    "add, get and delete rates" in test { rc =>
      val preconditions =
        Gen
          .listOfN(
            100,
            for {
              asset <- arbitraryIssuedAssetGen
              rateValue <- Gen.choose(1, 100).map(_.toDouble / 100)
            } yield asset -> rateValue
          )
          .map(_.toMap[Asset, Double])

      forAll(preconditions) { map =>
        map.foreach { case (asset, rateValue) => rc.upsertRate(asset, rateValue) shouldBe None }
        rc.getAllRates should matchTo(map ++ WavesRate)
        map.foreach { case (asset, rate) => rc.deleteRate(asset) shouldBe Some(rate) }
        rc.getAllRates should matchTo(WavesRate)
      }
    }

    "update rate if it already exists" in test { rc =>
      forAll(arbitraryIssuedAssetGen) { asset: Asset =>
        rc.upsertRate(asset, 1) shouldBe None
        rc.getAllRates should matchTo(Map(asset -> 1d) ++ WavesRate)

        rc.upsertRate(asset, 2) shouldBe Some(1d)
        rc.getAllRates should matchTo(Map(asset -> 2d) ++ WavesRate)

        rc.deleteRate(asset) shouldBe Some(2d)
      }
    }

    "correctly restore state from db" in {
      val rateDb = RateDb(asyncLevelDb)

      val asset1 = mkAssetId("First")
      val asset2 = mkAssetId("Second")

      rateDb.upsertRate(asset1, 1.5).futureValue
      rateDb.upsertRate(asset2, 5.1).futureValue

      RateCache(asyncLevelDb).futureValue.getAllRates should matchTo(Map(asset1 -> 1.5, asset2 -> 5.1) ++ WavesRate)
    }
  }
}
