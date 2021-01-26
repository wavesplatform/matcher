package com.wavesplatform.dex.actors.address

import com.wavesplatform.dex.collections.{NegativeMap, NonPositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

/**
 * We must treat a not observed transaction as a part of unconfirmed transactions until we observe it.
 *
 * This class helps to solve the case (see 3):
 * 1. We execute an order by a some amount
 * 2. We release the open volume by an executed amount
 * 3. The tradable balance becomes higher, that should be, because we haven't yet observed a transaction of OrderExecuted in UTX,
 *      thus unconfirmed is unchanged
 * 4. The transaction is observed in UTX
 * 5. Pessimistic portfolio increased (because of the unconfirmed part), thus the account gets the right tradable balance
 *
 * @param unconfirmed Includes all transactions (exchange, transfer, issue, etc.)
 * @param notObservedTxs Volume by ExchangeTransactions which haven't yet observed in streams
 * @param futureTxIds ExchangeTransactions which hasn't been registered as unconfirmed, but will be
 */
case class AddressPessimisticCorrection(
  unconfirmed: NonPositiveMap[Asset, Long],
  notObservedTxs: Map[ExchangeTransaction.Id, NegativeMap[Asset, Long]],
  futureTxIds: Set[ExchangeTransaction.Id]
) {

  /**
   * @param updates Should be non positive
   */
  def withInitUnconfirmed(updates: NonPositiveMap[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = updates ++ unconfirmed)

  /**
   * @param updates Should be non positive
   */
  def withFreshUnconfirmed(updates: NonPositiveMap[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = unconfirmed ++ updates)

  /**
   * @param expectedTxId Could be None if a transaction wasn't created or has been already created (see OrderEventsCoordinatorActor)
   * @return (updated, affected assets)
   */
  def withExecuted(
    expectedTxId: Option[ExchangeTransaction.Id],
    executionTotalVolumeDiff: NegativeMap[Asset, Long]
  ): (AddressPessimisticCorrection, Set[Asset]) = expectedTxId match {
    case None => (this, executionTotalVolumeDiff.keySet) // Won't expect withObserved
    case Some(txId) =>
      if (notObservedTxs.contains(txId)) throw new RuntimeException(s"$txId executed twice!")
      else if (futureTxIds.contains(txId)) (copy(futureTxIds = futureTxIds - txId), executionTotalVolumeDiff.keySet) // TODO Set
      else (copy(notObservedTxs = notObservedTxs.updated(txId, executionTotalVolumeDiff)), Set.empty) // Set.empty - see getBy
  }

  /**
   * Expected to call at most once for each txId.
   * A tx could appear twice: after appending to MemPool and after confirming in a new block.
   * But OrderEventsCoordinatorActor has a deduplication logic.
   * Even this happen, we just have a hanging txId in futureTxIds, which won't affect the process, only consumes small amount of memory.
   */
  def withObserved(txId: ExchangeTransaction.Id): (AddressPessimisticCorrection, Set[Asset]) =
    notObservedTxs.get(txId) match {
      case Some(v) => (copy(notObservedTxs = notObservedTxs.removed(txId)), v.keySet)
      case None => (copy(futureTxIds = futureTxIds + txId), Set.empty)
    }

  /**
   * @return A non-positive value
   */
  def getBy(asset: Asset): Long = unconfirmed.getOrElse(asset, 0L) + notObservedTxs.valuesIterator
    .flatMap(_.collect { case (`asset`, v) => v })
    .sum

}

object AddressPessimisticCorrection {
  val empty = AddressPessimisticCorrection(NonPositiveMap.empty, Map.empty, Set.empty)
}
