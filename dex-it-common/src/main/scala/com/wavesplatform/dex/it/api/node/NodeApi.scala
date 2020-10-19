package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress

import cats.tagless.{autoFunctorK, finalAlg}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.it.api.responses.node._
import im.mak.waves.transactions.Transaction
import im.mak.waves.transactions.common.Id

@finalAlg
@autoFunctorK
trait NodeApi[F[_]] {

  def wavesBalanceOrig(address: Address): F[WavesBalanceResponse]
  def assetBalanceOrig(address: Address, asset: IssuedAsset): F[AssetBalanceResponse]

  def broadcast(tx: Transaction): F[Unit]
  def transactionInfo(id: Id): F[Transaction]

  def currentHeightOrig: F[HeightResponse]

  def activationStatus: F[ActivationStatusResponse]

  def connect(toNode: InetSocketAddress): F[Unit]
  def connectedPeers: F[ConnectedPeersResponse]
}

object NodeApi {} // ClassNotFound NodeApi$ without this line
