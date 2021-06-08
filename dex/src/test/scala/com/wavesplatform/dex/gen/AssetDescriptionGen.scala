package com.wavesplatform.dex.gen

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

trait AssetDescriptionGen extends MatcherSpecBase { _: Suite =>

  protected def assertDescriptionsGen(n: Int): Gen[Map[Asset.IssuedAsset, BriefAssetDescription]] =
    Gen.containerOfN[Seq, (Asset.IssuedAsset, BriefAssetDescription)](n, assertDescriptionGen).map(_.toMap)

  protected val assertDescriptionGen: Gen[(Asset.IssuedAsset, BriefAssetDescription)] = for {
    asset <- issuedAssetGen(1.toByte)
    name <- Arbitrary.arbString.arbitrary
    decimals <- Gen.choose(0, 8)
    hasScript <- Arbitrary.arbBool.arbitrary
    isNft <- Gen.oneOf(true, false)
  } yield (asset, BriefAssetDescription(name, decimals, hasScript, isNft))

}
