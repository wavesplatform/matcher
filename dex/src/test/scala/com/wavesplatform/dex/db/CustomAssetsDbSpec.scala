package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.caches.OrderFeeSettingsCache.AssetsActionForOffset
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.MatcherSpecBase
import monix.execution.atomic.AtomicLong
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CustomAssetsDbSpec extends AnyFreeSpec with Matchers with WithDb with MatcherSpecBase with PropertyChecks {

  private val offsetLong = AtomicLong(0L)

  private val assetActionGen =
    for {
      assetsNumber <- Gen.choose[Int](1, 40)
      assets <- Gen.listOfN(assetsNumber, issuedAssetGen(1.toByte))
      isAdd <- Gen.oneOf(true, false)
    } yield AssetsActionForOffset(offsetLong.getAndIncrement(), assets.toSet, isAdd)

  "CustomFeeAssetsDb.levelDb" - {

    "save actions only once and read it" in forAll(assetActionGen) { action =>
      test { adb =>

        adb.save(action)
        val all = adb.all()
        all.size shouldBe 1
        all.headOption.get shouldBe action

        adb.save(action.copy(isAdded = !action.isAdded, assets = action.assets + Asset.Waves))
        val all2 = adb.all()
        all2.size shouldBe 1
        all2.headOption.get shouldBe action

      }
    }

    "save and read all values" in forAll(Gen.listOfN(50, assetActionGen)) { actions =>
      test { adb =>

        actions.foreach(adb.save)
        val all = adb.all()

        all.size shouldBe actions.size
        all.foreach { action =>
          actions.find(_.offset == action.offset).get shouldBe action
        }
      }
    }

  }

  private def test(f: CustomFeeAssetsDb[Id] => Any): Any = tempLevelDb(db => f(CustomFeeAssetsDb.levelDb(db)))

}
