package com.wavesplatform.it.config

import com.wavesplatform.dex.actors.OrderBookDirectoryActor
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.it.waves.MkWavesEntities.{mkIssue, mkIssueExtended, IssueResults}
import com.wavesplatform.dex.model.AssetPairBuilder
import com.wavesplatform.dex.waves.WavesFeeConstants.{issueFee, someAssetAmount}
import com.wavesplatform.transactions.IssueTransaction
import com.wavesplatform.dex.it.waves.ToWavesJConversions

import scala.util.Random

object DexTestConfig extends ToWavesJConversions {

  val orderLimit = 10

  def createAssetPair(asset1: String, asset2: String): AssetPair = {
    val (a1, a2) = (AssetPair.extractAsset(asset1).get, AssetPair.extractAsset(asset2).get)
    if (AssetPairBuilder.assetIdOrdering.compare(a1.compatId, a2.compatId) > 0)
      AssetPair(a1, a2)
    else
      AssetPair(a2, a1)
  }

  // TODO remove
  def createAssetPair(asset1: Asset, asset2: Asset): AssetPair =
    if (AssetPairBuilder.assetIdOrdering.compare(asset1.compatId, asset2.compatId) > 0) AssetPair(asset1, asset2)
    else AssetPair(asset2, asset1)

  def issueAssetPair(issuer: KeyPair, amountAssetDecimals: Byte, priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) =
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)

  @scala.annotation.tailrec
  def issueAssetPair(
    amountAssetIssuer: KeyPair,
    priceAssetIssuer: KeyPair,
    amountAssetDecimals: Byte,
    priceAssetDecimals: Byte
  ): (IssueTransaction, IssueTransaction, AssetPair) = {

    val IssueResults(issueAmountAssetTx, amountAssetId, amountAsset) =
      mkIssueExtended(amountAssetIssuer, Random.nextString(4), someAssetAmount, amountAssetDecimals, issueFee)

    val IssueResults(issuePriceAssetTx, priceAssetId, priceAsset) =
      mkIssueExtended(priceAssetIssuer, Random.nextString(4), someAssetAmount, priceAssetDecimals, issueFee)

    if (OrderBookDirectoryActor.compare(Some(priceAssetId.arr), Some(amountAssetId.arr)) < 0)
      (issueAmountAssetTx, issuePriceAssetTx, AssetPair(amountAsset, priceAsset))
    else issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  @scala.annotation.tailrec
  def assetPairIssuePriceAsset(issuer: KeyPair, amountAsset: Asset, priceAssetDecimals: Byte): (IssueTransaction, AssetPair) = {

    val issuePriceAssetTx = mkIssue(issuer, Random.nextString(4), someAssetAmount, priceAssetDecimals, issueFee)
    val priceAssetId: ByteStr = issuePriceAssetTx.id()
    val priceAsset = IssuedAsset(priceAssetId)

    if (OrderBookDirectoryActor.compare(Some(priceAssetId.arr), amountAsset.compatId.map(_.arr)) < 0)
      (issuePriceAssetTx, AssetPair(amountAsset, priceAsset))
    else assetPairIssuePriceAsset(issuer, amountAsset, priceAssetDecimals)
  }

}
