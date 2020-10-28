package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import monix.reactive.Observable

trait MatcherExtensionClient[F[_]] {

  def utxEvents: Observable[UtxEvent]

  def spendableBalances(address: Address, assets: Set[Asset]): F[Map[Asset, Long]]
  def allAssetsSpendableBalance(address: Address): F[Map[Asset, Long]]

  def isFeatureActivated(id: Short): F[Boolean]

  def assetDescription(asset: IssuedAsset): F[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): F[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): F[RunScriptResult]

  def hasScript(address: Address): F[Boolean]
  def runScript(address: Address, input: Order): F[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): F[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): F[Boolean]

  def forgedOrder(orderId: ByteStr): F[Boolean]

  def getNodeAddress: F[InetAddress]
  def currentHeight: F[Int]

  def close(): F[Unit]
}
