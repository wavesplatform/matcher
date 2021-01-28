package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits.getDiff

trait DiffMatcherWithImplicits extends DiffMatcher {

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(getDiff[ByteStr](_.toString == _.toString))
  implicit val derivedPublicKeyDiff: Derived[Diff[PublicKey]] = Derived(derivedByteStrDiff.contramap[PublicKey](_.arr))

  implicit val issuedAssetDiff: Diff[IssuedAsset] = { (left: IssuedAsset, right: IssuedAsset, _: List[FieldPath]) =>
    if (left.id == right.id) Identical(left) else DiffResultValue(left, right)
  }

  implicit val assetDiff: Diff[Asset] = { (left: Asset, right: Asset, _: List[FieldPath]) =>
    if (left == right) Identical(left) else DiffResultValue(left, right)
  }

  implicit val issuedAssetDerivedDiff: Derived[Diff[IssuedAsset]] = Derived(issuedAssetDiff)
  implicit val assetDerivedDiff: Derived[Diff[Asset]] = Derived(assetDiff)

}

object DiffMatcherWithImplicits {

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

}
