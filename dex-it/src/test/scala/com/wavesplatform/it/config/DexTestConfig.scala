package com.wavesplatform.it.config

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.AssetPairBuilder
import com.wavesplatform.dex.waves.WavesFeeConstants._
import com.wavesplatform.dex.market.MatcherActor
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.it.waves.MkWavesEntities
import com.wavesplatform.wavesj.transactions.IssueTransaction
import com.wavesplatform.dex.it.waves.Implicits._
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults

import scala.util.Random

object DexTestConfig extends MkWavesEntities {

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

  def issueAssetPair(issuer: KeyPair, amountAssetDecimals: Byte, priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)
  }

  @scala.annotation.tailrec
  def issueAssetPair(amountAssetIssuer: KeyPair,
                     priceAssetIssuer: KeyPair,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {

    val IssueResults(issueAmountAssetTx, amountAssetId, amountAsset) = {
      mkIssueExtended(amountAssetIssuer, Random.nextString(4), someAssetAmount, amountAssetDecimals, issueFee)
    }

    //    val issueAmountAssetTx: IssueTransactionV1 = IssueTransactionV1
//      .selfSigned(
//        sender = amountAssetIssuer,
//        name = Random.nextString(4).getBytes(),
//        description = Random.nextString(10).getBytes(),
//        quantity = someAssetAmount,
//        decimals = amountAssetDecimals,
//        reissuable = false,
//        fee = issueFee,
//        timestamp = System.currentTimeMillis()
//      )
//      .explicitGet()

    val IssueResults(issuePriceAssetTx, priceAssetId, priceAsset) = {
      mkIssueExtended(priceAssetIssuer, Random.nextString(4), someAssetAmount, priceAssetDecimals, issueFee)
    }

    //    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
//      .selfSigned(
//        sender = priceAssetIssuer,
//        name = Random.nextString(4).getBytes(),
//        description = Random.nextString(10).getBytes(),
//        quantity = someAssetAmount,
//        decimals = priceAssetDecimals,
//        reissuable = false,
//        fee = issueFee,
//        timestamp = System.currentTimeMillis()
//      )
//      .explicitGet()

    if (MatcherActor.compare(Some(priceAssetId.arr), Some(amountAssetId.arr)) < 0)
      (issueAmountAssetTx, issuePriceAssetTx, AssetPair(amountAsset, priceAsset))
    else issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  def assetPairIssuePriceAsset(issuer: KeyPair, amountAssetId: Asset, priceAssetDecimals: Byte): (IssueTransaction, AssetPair) = {
    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = issuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), amountAssetId.compatId.map(_.arr)) < 0) {
      (issuePriceAssetTx,
       AssetPair(
         amountAsset = amountAssetId,
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      assetPairIssuePriceAsset(issuer, amountAssetId, priceAssetDecimals)
  }
}
