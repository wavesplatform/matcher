package com.wavesplatform.dex.it.api.responses.node

import play.api.libs.json.{Format, Json}

case class NftAsset(
  assetId: String,
  issueHeight: Long,
  issueTimestamp: Long,
  issuer: String,
  name: String,
  description: String,
  decimals: Long,
  quantity: Long,
  scripted: Boolean,
  minSponsoredAssetFee: Option[Long],
  originTransactionId: Option[String]
)

object NftAsset {
  implicit val format: Format[NftAsset] = Json.format[NftAsset]
}
