package com.wavesplatform.dex.it.config

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.it.config.PredefinedAccounts._
import com.wavesplatform.dex.it.waves.MkWavesEntities.mkIssue
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair

trait PredefinedAssets {

  val defaultAssetQuantity = 999999999999L

  val usdAssetName = "USD-X"
  val IssueUsdTx   = mkIssue(alice, usdAssetName, defaultAssetQuantity, 2)
  val UsdId        = IssueUsdTx.id()
  val usd          = IssuedAsset(UsdId)

  val wctAssetName = "WCT-X"
  val IssueWctTx   = mkIssue(bob, wctAssetName, defaultAssetQuantity, 2)
  val WctId        = IssueWctTx.id()
  val wct          = IssuedAsset(WctId)

  val ethAssetName = "ETH-X"
  val IssueEthTx   = mkIssue(alice, ethAssetName, defaultAssetQuantity, 8)
  val EthId        = IssueEthTx.id()
  val eth          = IssuedAsset(EthId)

  val btcAssetName = "BTC-X"
  val IssueBtcTx   = mkIssue(bob, btcAssetName, defaultAssetQuantity, 8)
  val BtcId        = IssueBtcTx.id()
  val btc          = IssuedAsset(BtcId)

  val wctUsdPair   = AssetPair(wct, usd)
  val wctWavesPair = AssetPair(wct, Waves)
  val ethWavesPair = AssetPair(eth, Waves)
  val ethBtcPair   = AssetPair(eth, btc)
  val wavesUsdPair = AssetPair(Waves, usd)
  val ethUsdPair   = AssetPair(eth, usd)
  val wavesBtcPair = AssetPair(Waves, btc)

  val ForbiddenAssetId = ByteStr.decodeBase58("FdbnAsset").get
  val ForbiddenAsset   = IssuedAsset(ForbiddenAssetId)
}
