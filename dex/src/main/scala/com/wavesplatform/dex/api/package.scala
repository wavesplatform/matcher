package com.wavesplatform.dex

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.json.{assetDoubleMapFormat, assetMapFormat, assetPairMapFormat}
import com.wavesplatform.dex.queue.QueueEventWithMeta
import play.api.libs.json.{Format, JsValue, Writes}

package object api {

  type ApiMatcherPublicKey = PublicKey
  type ApiRates            = Map[Asset, Double]
  type ApiOffset           = QueueEventWithMeta.Offset
  type ApiSnapshotOffsets  = Map[AssetPair, ApiOffset]
  type ApiBalance          = Map[Asset, Long]

  implicit val apiMatcherPublicKeyFormat: Format[PublicKey]         = PublicKey.publicKeyJsonFormat
  implicit val apiRatesFormat: Format[ApiRates]                     = assetDoubleMapFormat
  implicit val apiSnapshotOffsetsFormat: Format[ApiSnapshotOffsets] = assetPairMapFormat[Long]
  implicit val apiBalanceFormat: Format[ApiBalance]                 = assetMapFormat[Long]

  implicit class ApiOps[A](private val self: A)(implicit writes: Writes[A]) {
    def toJson: JsValue = writes.writes(self)
  }
}
