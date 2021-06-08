package com.wavesplatform.dex.db

import cats.Id
import cats.syntax.option._
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.gen.AssetDescriptionGen
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AssetsDbSpec extends AnyFreeSpec with Matchers with AssetDescriptionGen with WithDb with PropertyChecks with NoShrink {

  "AssetsDb.levelDb implementation" - {
    "stores and reads all assets" in forAll(Gen.mapOf(assertDescriptionGen)) { assets =>
      test { adb: AssetsDb[Id] =>
        assets.foreach(Function.tupled(adb.put))
        assets.foreach {
          case (asset, desc) =>
            adb.get(asset) should matchTo(desc.some)
        }
      }
    }

  }

  private def test(f: AssetsDb[Id] => Any): Any = tempLevelDb(db => f(AssetsDb.levelDb(db)))

}
