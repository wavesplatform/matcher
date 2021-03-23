package com.wavesplatform.dex.api.http

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.json.{assetDoubleMapFormat, assetMapFormat, assetPairMapFormat}
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import play.api.libs.json.{Format, JsValue, Writes}

package object entities {

  type HttpMatcherPublicKey = PublicKey
  type HttpRates = Map[Asset, Double]
  type HttpOffset = ValidatedCommandWithMeta.Offset
  type HttpSnapshotOffsets = Map[AssetPair, HttpOffset]
  type HttpBalance = Map[Asset, Long]

  implicit val httpMatcherPublicKeyFormat: Format[PublicKey] = PublicKey.publicKeyJsonFormat
  implicit val httpRatesFormat: Format[HttpRates] = assetDoubleMapFormat
  implicit val httpSnapshotOffsetsFormat: Format[HttpSnapshotOffsets] = assetPairMapFormat[Long]
  implicit val httpBalanceFormat: Format[HttpBalance] = assetMapFormat[Long]

  implicit class HttpOps[A](private val self: A)(implicit writes: Writes[A]) {
    def toJson: JsValue = writes.writes(self)
  }

}
