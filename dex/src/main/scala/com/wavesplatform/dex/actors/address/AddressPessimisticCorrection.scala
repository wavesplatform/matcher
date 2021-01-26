package com.wavesplatform.dex.actors.address

import com.wavesplatform.dex.collections.{NegativeMap, NonPositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

/**
 * We must treat a not observed transaction as a part of unconfirmed transactions until we observe it.
 *
 * This class helps to solve the case (see 3):
 * 1. We execute an order by some amount
 * 2. We release the open volume by an executed amount
 * 3. The tradable balance becomes higher, that should be, because we haven't yet observed a transaction of OrderExecuted in UTX
 * 4. The transaction is observed in UTX
 * 5. Pessimistic portfolio increased, thus the account gets the right tradable balance
 *
 * @param unconfirmed Includes all transactions (exchange, transfer, issue, etc.)
 * @param notObserved Volume by ExchangeTransactions which haven't yet observed in streams
 * @param future ExchangeTransactions which hasn't been registered as unconfirmed, but will be
 */
case class AddressPessimisticCorrection(
  unconfirmed: NonPositiveMap[Asset, Long],
  notObserved: Map[ExchangeTransaction.Id, NegativeMap[Asset, Long]],
  future: Set[ExchangeTransaction.Id]
) {

  /**
   * @param updates Should be non positive
   */
  def withInit(updates: NonPositiveMap[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = updates ++ unconfirmed)

  /**
   * @param updates Should be non positive
   */
  def withFreshUnconfirmed(updates: NonPositiveMap[Asset, Long]): AddressPessimisticCorrection =
    copy(unconfirmed = unconfirmed ++ updates) // TODO becomes 0!

  /**
   * TODO with himself. volume by spending asset 0 ?
   * @param expectedTxId Could be None if a transaction wasn't created or has been already created (see OrderEventsCoordinatorActor)
   * @param executionTotalVolumeDiff Should be non negative
   */
  def withExecuted(
    expectedTxId: Option[ExchangeTransaction.Id],
    executionTotalVolumeDiff: Map[Asset, Long]
  ): (AddressPessimisticCorrection, Set[Asset]) =
    expectedTxId match {
      case None => (this, executionTotalVolumeDiff.keySet) // (copy(unconfirmed = unconfirmed |-| volume), volume.keySet) // - ?????
      case Some(txId) =>
        if (notObserved.contains(txId)) throw new RuntimeException(s"$txId executed twice!") // Could be called twice if one trader
        // If it among future, then unconfirmed is updated with withFreshUnconfirmed, (copy(unconfirmed = unconfirmed |-| volume, future = future - txId), volume.keySet) //
        else if (future.contains(txId)) (copy(future = future - txId), executionTotalVolumeDiff.keySet)
        // unconfirmed is updated only with withFreshUnconfirmed
        // else (copy(unconfirmed = unconfirmed |-| executionTotalVolumeDiff, notObserved = notObserved.updated(txId, executionTotalVolumeDiff)), Set.empty) // Set.empty - see getBy
        else
          (
            copy(notObserved = notObserved.updated(txId, NegativeMap(executionTotalVolumeDiff))), // TODO <---
            Set.empty
          ) // Set.empty - see getBy
    }

  // TODO changes with unconfirmed? not only, if aproved by broadcaster
  def withObserved(txId: ExchangeTransaction.Id): (AddressPessimisticCorrection, Set[Asset]) =
    notObserved.get(txId) match {
      case Some(v) => (copy(notObserved = notObserved.removed(txId)), v.keySet)
      case None =>
        // Could happen twice: after appending to MemPool and after confirming in a new block.
        // OrderEventsCoordinatorActor has the deduplication logic.
        // Even this happen, we just have a hanging txId in "future", which won't affect the process, only consumes small amount of memory.
        (copy(future = future + txId), Set.empty)
    }

  /**
   * @return A negative value
   */
  def getBy(asset: Asset): Long = unconfirmed.getOrElse(asset, 0L) + notObserved.valuesIterator
    .flatMap(_.collect { case (`asset`, v) => v })
    .sum

}

object AddressPessimisticCorrection {
  val empty = AddressPessimisticCorrection(NonPositiveMap.empty, Map.empty, Set.empty)
}
