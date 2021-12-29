package com.wavesplatform.dex.util

import com.google.protobuf.ByteString
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

trait Diffs {

  // diffx
  val byteStringDiff: Diff[ByteString] = Diff[String].contramap[ByteString](xs => Base58.encode(xs.toByteArray))

  implicit val derivedByteStringDiff: Derived[Diff[ByteString]] = Derived(byteStringDiff)
  implicit val derivedUtxTransactionDiff: Derived[Diff[UtxTransaction]] = Derived(byteStringDiff.contramap[UtxTransaction](_.id))

  implicit val addressDiff: Diff[Address] = Diff[String].contramap[Address](_.stringRepr)

  // TODO Duplicate
  implicit val issuedAssetDiff: Diff[IssuedAsset] = { (left: IssuedAsset, right: IssuedAsset, _: List[FieldPath]) =>
    if (left.id == right.id) Identical(left) else DiffResultValue(left, right)
  }

  // TODO Duplicate
  implicit val assetDiff: Diff[Asset] = { (left: Asset, right: Asset, _: List[FieldPath]) =>
    if (left == right) Identical(left) else DiffResultValue(left, right)
  }

  // TODO Duplicate
  implicit val issuedAssetDerivedDiff: Derived[Diff[IssuedAsset]] = Derived(issuedAssetDiff)

  // TODO Duplicate
  implicit val assetDerivedDiff: Derived[Diff[Asset]] = Derived(assetDiff)

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

}
