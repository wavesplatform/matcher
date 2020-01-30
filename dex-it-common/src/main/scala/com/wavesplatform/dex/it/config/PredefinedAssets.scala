package com.wavesplatform.dex.it.config

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.it.config.PredefinedAccounts._
import com.wavesplatform.dex.it.waves.Implicits._
import com.wavesplatform.dex.it.waves.MkWavesEntities.mkIssue
import com.wavesplatform.wavesj.transactions.IssueTransaction

trait PredefinedAssets {

  val defaultAssetQuantity: Long = 999999999999L

  val usdAssetName: String         = "USD-X"
  val IssueUsdTx: IssueTransaction = mkIssue(alice, usdAssetName, defaultAssetQuantity, 2)
  val UsdId: ByteStr               = toVanilla(IssueUsdTx.getId)
  val usd: IssuedAsset             = IssuedAsset(UsdId)

  val wctAssetName: String         = "WCT-X"
  val IssueWctTx: IssueTransaction = mkIssue(bob, wctAssetName, defaultAssetQuantity, 2)
  val WctId: ByteStr               = toVanilla(IssueWctTx.getId)
  val wct: IssuedAsset             = IssuedAsset(WctId)

  val ethAssetName: String         = "ETH-X"
  val IssueEthTx: IssueTransaction = mkIssue(alice, ethAssetName, defaultAssetQuantity, 8)
  val EthId: ByteStr               = toVanilla(IssueEthTx.getId)
  val eth: IssuedAsset             = IssuedAsset(EthId)

  val btcAssetName: String         = "BTC-X"
  val IssueBtcTx: IssueTransaction = mkIssue(bob, btcAssetName, defaultAssetQuantity, 8)
  val BtcId: ByteStr               = toVanilla(IssueBtcTx.getId)
  val btc: IssuedAsset             = IssuedAsset(BtcId)

  val wctUsdPair: AssetPair   = AssetPair(wct, usd)
  val wctWavesPair: AssetPair = AssetPair(wct, Waves)
  val ethWavesPair: AssetPair = AssetPair(eth, Waves)
  val ethBtcPair: AssetPair   = AssetPair(eth, btc)
  val wavesUsdPair: AssetPair = AssetPair(Waves, usd)
  val ethUsdPair: AssetPair   = AssetPair(eth, usd)
  val wavesBtcPair: AssetPair = AssetPair(Waves, btc)

  val ForbiddenAssetId: ByteStr   = ByteStr.decodeBase58("FdbnAsset").get
  val ForbiddenAsset: IssuedAsset = IssuedAsset(ForbiddenAssetId)

  implicit val assetDecimalsMap: Map[Asset, Int] = Map[Asset, Int](Waves -> 8, usd -> 2, wct -> 2, eth -> 8, btc -> 8).withDefaultValue(8)
}
