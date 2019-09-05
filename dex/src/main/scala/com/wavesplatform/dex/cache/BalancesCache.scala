package com.wavesplatform.dex.cache

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset

class BalancesCache(getFromBlockchain: (Address, Asset) => Long) {

  private val balancesCache = new ConcurrentHashMap[(Address, Asset), Long](1000, 0.9f, 10)

  def get(key: (Address, Asset)): Long = if (balancesCache containsKey key) balancesCache get key else getFromBlockchain(key._1, key._2)

  def updateAllValues(): Unit = balancesCache.keySet forEach (_ => getFromBlockchain)

  def upsert(key: (Address, Asset), value: Long): Unit = balancesCache.put(key, value)

  def batchUpsert(batch: Map[Address, Map[Asset, Long]]): Unit = {
    batch.foreach {
      case (address, changedBalances) => changedBalances.foreach { case (asset, balance) => upsert(address -> asset, balance) }
    }
  }
}
