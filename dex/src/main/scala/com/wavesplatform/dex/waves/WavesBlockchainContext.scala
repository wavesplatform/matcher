package com.wavesplatform.dex.waves

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.{AssetDescription, VolumeAndFee}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.reactive.Observable

trait WavesBlockchainContext {
  def hasTx(tx: Transaction): Boolean
  def broadcastTx(tx: Transaction): Unit

  def isFeatureActivated(id: Short): Boolean

  def assetDescription(asset: IssuedAsset): Option[AssetDescription]

  def hasScript(asset: IssuedAsset): Boolean
  def runScript(asset: IssuedAsset, input: Transaction): Either[String, Terms.EVALUATED]

  def hasScript(address: Address): Boolean
  def runScript(address: Address, input: Order): Either[String, Terms.EVALUATED]

  def spendableBalanceChanged: Observable[(Address, Asset)]
  def spendableBalance(address: Address, asset: Asset): Long

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee
  def putToUtx(tx: Transaction): Boolean
}
