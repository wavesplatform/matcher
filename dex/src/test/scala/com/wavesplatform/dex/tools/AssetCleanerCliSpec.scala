package com.wavesplatform.dex.tools

import com.wavesplatform.dex.db.{AssetPairsDb, AssetsDb, WithDb}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.gen.AssetDescriptionGen
import com.wavesplatform.dex.tool.AssetCacheCleaner
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetCleanerCliSpec extends AnyFreeSpec with AssetDescriptionGen with WithDb with Matchers with EitherValues {

  "AssetCacheCleaner" in {

    val assetDescriptions = assertDescriptionsGen(5).sample.get
    val assetPairs = Gen.containerOf[Set, AssetPair](assetPairGen).sample.get

    tempLevelDb { levelDb =>
      val apDb = AssetPairsDb.levelDb(levelDb)
      val adDb = AssetsDb.levelDb(levelDb)

      markup("adding some assets for db cache")
      assetDescriptions.foreach { case (asset, description) =>
        adDb.put(asset, description)
      }

      markup("adding some asset pairs for db cache")
      assetPairs.foreach(apDb.add)

      markup("successfully clean all BriefAssetDescription")
      AssetCacheCleaner.cleanAssets(levelDb).isRight shouldBe true

      assetDescriptions.foreach { case (asset, _) =>
        adDb.get(asset) shouldBe None
      }

      markup("leave all asset pairs untouched")
      apDb.all() shouldBe assetPairs

    }

  }

}
