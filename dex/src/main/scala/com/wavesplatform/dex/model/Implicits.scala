package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.asset.AssetPair

import scala.util.{Failure, Try}

object Implicits {

  implicit final class AssetPairOps(val self: AssetPair.type) extends AnyVal {

    def fromString(s: String): Try[AssetPair] = Try(s.split("-")).flatMap {
      case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
      case xs => Failure(new Exception(s"$s (incorrect assets count, expected 2 but got ${xs.size}: ${xs.mkString(", ")})"))
    }

  }

}
