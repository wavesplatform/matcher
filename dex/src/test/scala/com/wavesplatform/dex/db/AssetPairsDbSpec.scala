package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AssetPairsDbSpec extends AnyFreeSpec with Matchers with WithDb with MatcherSpecBase with PropertyChecks with NoShrink {

  private val fixedAssetPairGen = assetPairGen.filterNot(x => x.amountAsset == Waves && x.priceAsset == Waves)

  "Default AssetPairsDB implementation" - {
    "stores and reads all asset pairs" in {
      val g = Gen.containerOf[Set, AssetPair](fixedAssetPairGen)
      forAll(g) { assetPairs =>
        test { apdb =>
          assetPairs.foreach(apdb.add)
          apdb.all() shouldBe assetPairs
        }
      }
    }

    "removes asset pair" in {
      val g = for {
        xs <- Gen.nonEmptyContainerOf[Vector, AssetPair](fixedAssetPairGen)
        indexGen = Gen.choose(0, xs.size - 1)
        is <- Gen.nonEmptyListOf(indexGen)
      } yield (xs.toSet, is.toSet.map(xs.apply))

      forAll(g) {
        case (assetPairs, toRemove) =>
          test { apdb =>
            assetPairs.foreach(apdb.add)
            toRemove.foreach(apdb.remove)
            apdb.all() shouldBe (assetPairs -- toRemove)
          }
      }
    }

  }

  private def test(f: AssetPairsDb[Id] => Any): Any = tempLevelDb(db => f(AssetPairsDb.levelDb(db)))

}
