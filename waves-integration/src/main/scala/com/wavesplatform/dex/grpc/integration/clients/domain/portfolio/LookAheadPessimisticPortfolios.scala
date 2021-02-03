package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

import scala.collection.mutable
import scala.util.chaining._

/**
 * Caches unknown forged transactions and don't add them in pending.
 * This class is needed, because we have two separate streams: Blockchain and UTX events.
 * Solves the rare case, when we get a new block event with a new transaction before UTX events with this transaction.
 * In other words, if we run processConfirmed for a transaction, we won't call addPending for the same transaction after.
 */
class LookAheadPessimisticPortfolios(orig: PessimisticPortfolios, maxConfirmedTransactions: Int) extends PessimisticPortfolios {

  // TODO FifoSet
  private val confirmedTxsEvictionQueue = new mutable.Queue[ByteString](maxConfirmedTransactions)
  private val confirmedTxs = new mutable.HashSet[ByteString]

  override def getAggregated(address: Address): Map[Asset, Long] = orig.getAggregated(address)

  override def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address] = {
    confirmedTxs.clear()
    confirmedTxsEvictionQueue.clear()
    orig.replaceWith(setTxs)
  }

  override def addPending(txs: Iterable[PessimisticTransaction]): Set[Address] =
    orig.addPending(txs.filterNot(remove)) // Without unknown

  /**
   * @return (affected addresses, unknown transactions)
   */
  override def processConfirmed(txIds: Iterable[ByteString]): (Set[Address], List[ByteString]) =
    // We don't filter, because a transaction can't be forged twice
    orig.processConfirmed(txIds).tap { case (_, unknownTxIds) =>
      unknownTxIds.foreach(put)
    }

  override def removeFailed(txIds: Iterable[ByteString]): Set[Address] =
    // txIds.foreach(remove) // a transaction can't be forged and failed both.
    orig.removeFailed(txIds)

  private def put(txId: ByteString): Unit = confirmedTxs.add(txId).tap { added =>
    if (added) {
      if (confirmedTxsEvictionQueue.size == maxConfirmedTransactions) confirmedTxsEvictionQueue.removeLastOption().foreach(confirmedTxs.remove)
      confirmedTxsEvictionQueue.enqueue(txId)
    }
  }

  private def remove(tx: PessimisticTransaction): Boolean = remove(tx.txId)

  private def remove(id: ByteString): Boolean = confirmedTxs.remove(id).tap { had =>
    if (had) confirmedTxsEvictionQueue.removeFirst(_ == id)
  }

}
