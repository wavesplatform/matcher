package com.wavesplatform.dex

import cats.Show
import cats.Show.{show, fromToString => autoShow}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._

package object error {
  implicit val byteWrites              = Writes.IntWrites.contramap[Byte](_.toInt)
  implicit val byteStrWrites           = Writes.StringWrites.contramap[ByteStr](_.base58)
  implicit val assetWrites             = Writes.StringWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = Writes[AssetPair](_.json)
  implicit val publicKeyWrites         = Writes.StringWrites.contramap[PublicKey](_.base58)
  implicit val addressWrites           = Writes.StringWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = Writes.StringWrites.contramap[BlockchainFeature](_.description)

  implicit val byteShow              = autoShow[Byte]
  implicit val intShow               = autoShow[Int]
  implicit val longShow              = autoShow[Long]
  implicit val doubleShow            = autoShow[Double]
  implicit val stringShow            = show[String](identity)
  implicit val byteStrShow           = show[ByteStr](_.base58)
  implicit val assetShow             = show[Asset](AssetPair.assetIdStr)
  implicit val issuedAssetShow       = show[IssuedAsset](AssetPair.assetIdStr)
  implicit val assetPairShow         = show[AssetPair](_.key)
  implicit val publicKeyShow         = show[PublicKey](_.base58)
  implicit val addressShow           = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow = show[BlockchainFeature](_.description)

  private def showCol[C[_], T](f: C[T] => List[T])(implicit itemShow: Show[T]): Show[C[T]] =
    (input: C[T]) => s"{${f(input).map(itemShow.show).mkString(", ")}}"
  implicit def setShow[T](implicit itemShow: Show[T]): Show[Set[T]]   = showCol[Set, T](_.toList)
  implicit def listShow[T](implicit itemShow: Show[T]): Show[List[T]] = showCol[List, T](_.toList)

  implicit def mapShow[K, V](implicit kShow: Show[K], vShow: Show[V]): Show[Map[K, V]] = { input =>
    val xs = input.map { case (k, v) => s"${kShow.show(k)}: ${vShow.show(v)}" }
    s"{${xs.mkString(", ")}}"
  }
}
