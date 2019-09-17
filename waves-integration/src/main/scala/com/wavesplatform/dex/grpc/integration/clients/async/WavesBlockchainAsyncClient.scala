package com.wavesplatform.dex.grpc.integration.clients.async

import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.transaction.Asset
import monix.reactive.Observable

import scala.concurrent.Future

object WavesBlockchainAsyncClient {
  type SpendableBalance        = Map[Asset, Long]
  type SpendableBalanceChanges = Map[Address, SpendableBalance]
}

trait WavesBlockchainAsyncClient {
  def spendableBalanceChanges: Observable[SpendableBalanceChanges]
  def spendableBalance(address: Address, asset: Asset): Future[Long]
}
