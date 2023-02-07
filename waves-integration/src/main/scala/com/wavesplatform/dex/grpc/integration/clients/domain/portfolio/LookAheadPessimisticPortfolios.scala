package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.google.protobuf.ByteString
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

/**
 * Caches unknown forged transactions and don't add them in pending.
 * This class is needed, because we have two separate streams: Blockchain and UTX events.
 * Solves the rare case, when we get a new block event with a new transaction before UTX events with this transaction.
 * In other words, if we run processConfirmed for a transaction, we won't call addPending for the same transaction after.
 */
class LookAheadPessimisticPortfolios(orig: PessimisticPortfolios, maxConfirmedTransactions: Int) extends PessimisticPortfolios {

  private val confirmedTxs = FifoSet.limited[ByteString](maxConfirmedTransactions)

  override def getAggregated(address: Address): Map[Asset, Long] = orig.getAggregated(address)

  override def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address] = {
    confirmedTxs.clear()
    orig.replaceWith(setTxs)
  }

  override def addPending(txs: Iterable[PessimisticTransaction]): Set[Address] = {
    val newTxs = txs.filterNot(tx => confirmedTxs.contains(tx.txId))
    orig.addPending(newTxs) // Without unknown
  }

  /**
   * @return (affected addresses, unknown transactions)
   */
  override def processConfirmed(txIds: Iterable[ByteString]): (Set[Address], List[ByteString]) = {
    val newTxIds = txIds.filterNot(confirmedTxs.contains)
    newTxIds.foreach(confirmedTxs.append)
    orig.processConfirmed(newTxIds)
  }

  override def removeFailed(txIds: Iterable[ByteString]): Set[Address] =
    orig.removeFailed(txIds)

}
