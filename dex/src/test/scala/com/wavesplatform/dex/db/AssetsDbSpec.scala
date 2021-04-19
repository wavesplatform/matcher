package com.wavesplatform.dex.db

import cats.Id
import cats.syntax.option._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AssetsDbSpec extends AnyFreeSpec with Matchers with WithDb with MatcherSpecBase with PropertyChecks with NoShrink {

  private val assetDescPairGen = for {
    asset <- issuedAssetGen(1.toByte)
    name <- Arbitrary.arbString.arbitrary
    decimals <- Gen.choose(0, 8)
    hasScript <- Arbitrary.arbBool.arbitrary
  } yield (asset, BriefAssetDescription(name, decimals, hasScript))

  "AssetsDb.levelDb implementation" - {
    "stores and reads all assets" in forAll(Gen.mapOf(assetDescPairGen)) { assets =>
      test { adb =>
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
