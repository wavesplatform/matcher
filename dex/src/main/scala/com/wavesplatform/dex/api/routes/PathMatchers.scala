package com.wavesplatform.dex.api.routes

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.{PathMatcher, PathMatcher1, PathMatchers => AkkaMatchers}
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.error.ValidationError.{InvalidAddress, InvalidAsset, InvalidBase58String, InvalidPublicKey}

object PathMatchers {

  class Base58[A](f: String => Option[A]) extends PathMatcher1[A] {

    def apply(path: Path): PathMatcher.Matching[Tuple1[A]] = path match {
      case Path.Segment(segment, tail) => f(segment).fold[PathMatcher.Matching[Tuple1[A]]](Unmatched)(v => Matched(tail, Tuple1(v)))
      case _ => Unmatched
    }

  }

  val AssetPairPM: PathMatcher1[Either[InvalidAsset, AssetPair]] = AkkaMatchers.Segments(2).flatMap {
    case a1 :: a2 :: Nil =>
      Option(AssetPair.createAssetPair(a1, a2).toEither.left.map { ex =>
        val m = ex.getMessage
        if (m.contains(a1)) InvalidAsset(a1, m) else InvalidAsset(a2, m)
      })
    case _ => Option(Left(InvalidAsset(null, "Unexpected error")))
  }

  object AssetPM
      extends Base58[Either[InvalidAsset, Asset]](s => Option(AssetPair.extractAsset(s).toEither.left.map(ex => InvalidAsset(s, ex.getMessage))))

  object OrderPM
      extends Base58[Either[InvalidBase58String, ByteStr]](s =>
        Option(ByteStr.decodeBase58(s).toEither.left.map(ex => InvalidBase58String(ex.getMessage)))
      )

  object PublicKeyPM extends Base58[Either[InvalidPublicKey, PublicKey]](s => Option(PublicKey fromBase58String s))

  object AddressPM extends Base58[Either[InvalidAddress, Address]](s => Option(Address fromString s))
}
