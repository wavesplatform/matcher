package com.wavesplatform.dex.it.api.node

import cats.tagless.{autoFunctorK, finalAlg}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.it.api.responses.node._
import com.wavesplatform.transactions.Transaction
import com.wavesplatform.transactions.common.Id
import play.api.libs.json.JsValue

import java.net.InetSocketAddress

@finalAlg
@autoFunctorK
trait NodeApi[F[_]] {

  def wavesBalanceOrig(address: Address): F[WavesBalanceResponse]
  def assetBalanceOrig(address: Address, asset: IssuedAsset): F[AssetBalanceResponse]

  def assetsBalance(address: Address): F[AssetsBalancesResponse]
  def nftAssetsByAddress(address: Address, limit: Long = 999L): F[Seq[NftAsset]]

  def broadcast(tx: Transaction): F[Unit]
  def broadcastEth(ethTx: Array[Byte]): F[Unit]
  def unconfirmedTransactions: F[List[Transaction]]
  def transactionInfo(id: Id): F[Transaction]
  def unconfirmedTransactionInfo(id: Id): F[Transaction]

  def currentHeightOrig: F[HeightResponse]

  def activationStatus: F[ActivationStatusResponse]

  def connect(toNode: InetSocketAddress): F[Unit]
  def connectedPeers: F[ConnectedPeersResponse]

  def rollback(toHeight: Int, returnTransactionsToUtx: Boolean): F[Unit]

  def print(message: String): F[Unit]

  //TODO should be removed after wavesj update
  def broadcastRaw(js: JsValue): F[Unit]
}

object NodeApi {} // ClassNotFound NodeApi$ without this line
