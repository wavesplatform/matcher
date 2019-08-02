package com.wavesplatform.dex.grpc.integration.config

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.config.Accounts._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.assets.exchange.AssetPair

object Assets {

  SetupAddressScheme.setup()

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"
  val btcAssetName = "BTC-X"

  val defaultAssetQuantity = 999999999999L

  val IssueUsdTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueEthTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = ethAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueBtcTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = btcAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val BtcId = IssueBtcTx.id()
  val EthId = IssueEthTx.id()
  val UsdId = IssueUsdTx.id()
  val WctId = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = IssuedAsset(WctId),
    priceAsset = IssuedAsset(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = IssuedAsset(WctId),
    priceAsset = Waves
  )

  val ethWavesPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = Waves
  )

  val ethBtcPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(BtcId)
  )

  val wavesUsdPair = AssetPair(
    amountAsset = Waves,
    priceAsset = IssuedAsset(UsdId)
  )

  val ethUsdPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(UsdId)
  )

  val wavesBtcPair = AssetPair(
    amountAsset = Waves,
    priceAsset = IssuedAsset(BtcId)
  )

}
