package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset

import scala.concurrent.{ExecutionContext, Future}

class BalancesCache1(load: (Address, Asset) => Future[Long], expiration: Duration)(implicit executionContext: ExecutionContext)
    extends CacheWithExpiration[Future, (Address, Asset), BigInt](
      load = { case (address, asset) => load(address, asset) map BigInt.apply },
      expiration = expiration
    ) {

  def batchPut(batch: Map[Address, Map[Asset, Long]]): Unit = {
    batch.foreach {
      case (address, changedBalances) => changedBalances.foreach { case (asset, balance) => put(address -> asset, BigInt(balance)) }
    }
  }
}
