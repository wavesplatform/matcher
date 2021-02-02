package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits._
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios.Settings
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

class SynchronizedPessimisticPortfolios(settings: Settings) {

  private val orig = new LookAheadPessimisticPortfolios(new DefaultPessimisticPortfolios(), settings.maxConfirmedTransactions)

  private val reentrantLock = new ReentrantReadWriteLock()

  private def read[T](f: => T): T =
    try { reentrantLock.readLock().lock(); f }
    finally reentrantLock.readLock().unlock()

  private def write[T](f: => T): T =
    try { reentrantLock.writeLock().lock(); f }
    finally reentrantLock.writeLock().unlock()

  // TODO DEX-1013
  def replaceWith(setTxs: Seq[UtxTransaction]): Set[Address] = write {
    orig.replaceWith(setTxs.map(x => PessimisticTransaction(x.id, x.pessimisticPortfolios)))
  }

  // TODO DEX-1013
  /**
   * @return (affected addresses, unknown transactions)
   */
  def processConfirmed(txIds: Set[ByteString]): (Set[Address], List[ByteString]) = write {
    orig.processConfirmed(txIds)
  }

  // TODO DEX-1013
  def removeFailed(txIds: Set[ByteString]): Set[Address] = write {
    orig.removeFailed(txIds)
  }

  // TODO DEX-1013
  def addPending(txs: Seq[UtxTransaction]): Set[Address] = write {
    orig.addPending(txs.map(x => PessimisticTransaction(x.id, x.pessimisticPortfolios)))
  }

  def getAggregated(address: Address): Map[Asset, Long] = read(orig.getAggregated(address))

}

object SynchronizedPessimisticPortfolios {
  case class Settings(maxConfirmedTransactions: Int)
}
