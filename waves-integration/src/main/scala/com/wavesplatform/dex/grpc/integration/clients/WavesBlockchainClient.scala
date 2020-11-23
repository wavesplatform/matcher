package com.wavesplatform.dex.grpc.integration.clients

import cats.Monoid
import com.wavesplatform.dex.collection.MapOps.Ops2D
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable

import scala.concurrent.Future

trait WavesBlockchainClient {
  def updates: Observable[WavesBlockchainClient.Updates]

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]

  def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[Boolean]

  def forgedOrder(orderId: ByteStr): Future[Boolean]

  def close(): Future[Unit]
}

object WavesBlockchainClient {

  type BalanceChanges = Map[Address, Map[Asset, Long]]
  case class Updates(updatedBalances: Map[Address, Map[Asset, Long]])

  object Updates {

    implicit val updatesMonoid: Monoid[Updates] = new Monoid[Updates] {
      override val empty: Updates = Updates(Map.empty)

      override def combine(x: Updates, y: Updates): Updates = Updates(
        x.updatedBalances.deepReplace(y.updatedBalances)
      )

    }

  }

}
