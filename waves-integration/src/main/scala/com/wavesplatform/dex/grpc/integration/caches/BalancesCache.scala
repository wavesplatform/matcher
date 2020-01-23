package com.wavesplatform.dex.grpc.integration.caches

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

import scala.concurrent.{ExecutionContext, Future}

class BalancesCache(loader: (Address, Asset) => Future[Long])(implicit executionContext: ExecutionContext)
    extends BlockchainCache[(Address, Asset), BigInt](
      loader = { case (address, asset) => loader(address, asset) map BigInt.apply },
      expiration = None,
      invalidationPredicate = BlockchainCache.noCustomInvalidationLogic
    ) {

  def batchPut(batch: Map[Address, Map[Asset, Long]]): Unit = {
    batch.foreach {
      case (address, changedBalances) =>
        changedBalances.foreach { case (asset, balance) => put(address -> asset, Future.successful { BigInt(balance) }) }
    }
  }
}
