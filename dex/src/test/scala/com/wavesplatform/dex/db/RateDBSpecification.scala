package com.wavesplatform.dex.db

import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class RateDBSpecification extends AnyWordSpecLike with Matchers with WithDB with MatcherSpecBase with PropertyChecks with NoShrink {

  private def test(f: RateDB => Unit): Unit = f { RateDB(db) }

  "RateDB" should {
    "add, get and delete rates" in test { rdb =>
      val preconditions =
        Gen
          .listOfN(
            100,
            for {
              asset     <- arbitraryAssetGen
              rateValue <- Gen.choose(1, 100).map(_.toDouble / 100)
            } yield asset -> rateValue
          )
          .map(_.toMap)

      forAll(preconditions) { map =>
        map.foreach { case (asset, rateValue) => rdb.upsertRate(asset, rateValue) }
        rdb.getAllRates shouldBe map
        map.foreach { case (asset, _) => rdb.deleteRate(asset) }
        rdb.getAllRates.size shouldBe 0
      }
    }

    "update rate if it already exists" in test { rdb =>
      forAll(arbitraryAssetGen) { asset =>
        rdb.upsertRate(asset, 1)
        rdb.getAllRates shouldBe Map(asset -> 1)

        rdb.upsertRate(asset, 2)
        rdb.getAllRates shouldBe Map(asset -> 2)

        rdb.deleteRate(asset)
      }
    }
  }
}
