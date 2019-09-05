package com.wavesplatform.dex.grpc.integration.clients.async

import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBalancesClient.SpendableBalanceChanges
import com.wavesplatform.transaction.Asset
import monix.reactive.Observable

object WavesBalancesClient {
  type SpendableBalance        = Map[Asset, Long]
  type SpendableBalanceChanges = Map[Address, SpendableBalance]
}

trait WavesBalancesClient {
  def spendableBalanceChanges: Observable[SpendableBalanceChanges]
}
