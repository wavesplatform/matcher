package com.wavesplatform.dex.db

import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.Future

class RateDbSpecification extends AnyWordSpecLike with Matchers with WithDb with MatcherSpecBase with PropertyChecks with NoShrink {

  private def test(f: RateDb[Future] => Unit): Unit = f(RateDb(asyncLevelDb))

  "RateDb" should {
    "add, get and delete rates" in test { rdb =>
      val preconditions =
        Gen
          .listOfN(
            100,
            for {
              asset <- arbitraryIssuedAssetGen
              rateValue <- Gen.choose(1, 100).map(_.toDouble / 100)
            } yield asset -> rateValue
          )
          .map(_.toMap)

      forAll(preconditions) { map =>
        map.foreach { case (asset, rateValue) => rdb.upsertRate(asset, rateValue).futureValue }
        rdb.getAllRates.futureValue shouldBe map
        map.foreach { case (asset, _) => rdb.deleteRate(asset).futureValue }
        rdb.getAllRates.futureValue.size shouldBe 0
      }
    }

    "update rate if it already exists" in test { rdb =>
      forAll(arbitraryIssuedAssetGen) { asset =>
        rdb.upsertRate(asset, 1).futureValue
        rdb.getAllRates.futureValue shouldBe Map(asset -> 1)

        rdb.upsertRate(asset, 2).futureValue
        rdb.getAllRates.futureValue shouldBe Map(asset -> 2)

        rdb.deleteRate(asset).futureValue
      }
    }
  }
}
