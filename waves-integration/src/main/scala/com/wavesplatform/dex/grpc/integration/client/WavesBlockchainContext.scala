package com.wavesplatform.dex.grpc.integration.client

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.client.WavesBlockchainContext.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import monix.reactive.Observable

trait WavesBlockchainContext {
  // TODO multiple ids?
  def wasForged(id: ByteStr): Boolean
  def broadcastTx(txs: ExchangeTransaction): Boolean

  def isFeatureActivated(id: Short): Boolean

  def assetDescription(asset: IssuedAsset): Option[BriefAssetDescription]

  def hasScript(asset: IssuedAsset): Boolean
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): RunScriptResult

  def hasScript(address: Address): Boolean
  def runScript(address: Address, input: Order): RunScriptResult

  def spendableBalanceChanged: Observable[(Address, Asset)]
  def spendableBalance(address: Address, asset: Asset): Long

  def forgedOrder(orderId: ByteStr): Boolean
}

object WavesBlockchainContext {
  sealed trait RunScriptResult
  object RunScriptResult {
    case class ScriptError(message: String)             extends RunScriptResult
    case object Denied                                  extends RunScriptResult
    case object Allowed                                 extends RunScriptResult
    case class UnexpectedResult(rawResult: String)      extends RunScriptResult
    case class Exception(name: String, message: String) extends RunScriptResult
  }
}
