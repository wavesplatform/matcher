package com.wavesplatform.dex.cache

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

class BalancesCache(getFromBlockchain: (Address, Asset) => Future[Long])(implicit executionContext: ExecutionContext) extends ScorexLogging {

  private val balancesCache = new ConcurrentHashMap[(Address, Asset), Long](1000, 0.9f, 10)

  def get(key: (Address, Asset)): Future[Long] = {
    if (balancesCache containsKey key) Future.successful { balancesCache get key } else {
      getFromBlockchain(key._1, key._2).map { balance =>
        upsert(key, balance)
        balance
      }
    }
  }

  def updateAllValues(): Unit = balancesCache.keySet.asScala.foreach(_ => getFromBlockchain)

  def upsert(key: (Address, Asset), value: Long): Unit = {
    log.trace(s"upsert $key: $value")
    balancesCache.put(key, value)
  }

  def batchUpsert(batch: Map[Address, Map[Asset, Long]]): Unit = {
    batch.foreach {
      case (address, changedBalances) => changedBalances.foreach { case (asset, balance) => upsert(address -> asset, balance) }
    }
  }
}
