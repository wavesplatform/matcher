package com.wavesplatform.dex.it.api.responses.node

import com.wavesplatform.dex.it.api.responses.node.AssetsBalancesResponse.AssetBalance
import play.api.libs.json.{Format, JsObject, Json}

case class AssetsBalancesResponse(address: String, balances: Seq[AssetBalance])

object AssetsBalancesResponse {
  implicit val format: Format[AssetsBalancesResponse] = Json.format[AssetsBalancesResponse]

  case class AssetBalance(
    assetId: String,
    balance: Long,
    reissuable: Boolean,
    minSponsoredAssetFee: Option[Long],
    sponsorBalance: Option[Long],
    quantity: Long,
    issueTransaction: Option[JsObject]
  )

  object AssetBalance {
    implicit val format: Format[AssetBalance] = Json.format[AssetBalance]
  }

}
