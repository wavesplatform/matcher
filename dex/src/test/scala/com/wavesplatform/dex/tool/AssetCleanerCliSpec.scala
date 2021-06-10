package com.wavesplatform.dex.tool

import com.wavesplatform.dex.cli.WavesDexCli
import com.wavesplatform.dex.db.{AssetPairsDb, AssetsDb, WithDb}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.gen.AssetDescriptionGen
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetCleanerCliSpec extends AnyFreeSpec with AssetDescriptionGen with WithDb with Matchers with EitherValues {

  "AssetCacheCleaner" in {

    val assetDescriptionsCount = 5
    val assetDescriptions = assertDescriptionsGen(assetDescriptionsCount).sample.get
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
      WavesDexCli.cleanAssets(levelDb) shouldBe assetDescriptionsCount

      assetDescriptions.foreach { case (asset, _) =>
        adDb.get(asset) shouldBe None
      }

      markup("leave all asset pairs untouched")
      apDb.all() shouldBe assetPairs

    }

  }

}
