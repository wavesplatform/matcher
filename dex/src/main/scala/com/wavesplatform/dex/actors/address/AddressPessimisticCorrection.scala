package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.fp.MapImplicits.group

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
 * @param notObserved Volume by ExchangeTransactions which haven't yet observed in streams. Contains only positive values
 * @param unconfirmed Includes all transactions (exchange, transfer, issue, etc.). Contains only negative values
 * @param future ExchangeTransactions which hasn't been registered as unconfirmed, but will be
 */
case class AddressPessimisticCorrection(
  notObserved: Map[ExchangeTransaction.Id, Map[Asset, Long]],
  unconfirmed: Map[Asset, Long],
  future: Set[ExchangeTransaction.Id]
) {

  /**
   * @param updates Should be non positive
   */
  def withInit(updates: Map[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = updates ++ unconfirmed)

  /**
   * @param updates Should be non positive
   */
  def withFreshUnconfirmed(updates: Map[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = unconfirmed ++ updates)

  /**
   * TODO with himself. volume by spending asset 0 ?
   * @param volume Should be non negative
   */
  def withExecuted(txId: ExchangeTransaction.Id, volume: Map[Asset, Long]): AddressPessimisticCorrection =
    if (notObserved.contains(txId)) throw new RuntimeException(s"$txId executed twice!")
    else if (future.contains(txId)) copy(
      unconfirmed = unconfirmed |-| volume,
      future = future - txId
    )
    else copy(
      unconfirmed = unconfirmed |-| volume,
      notObserved = notObserved.updated(txId, volume)
    )

  def withObserved(txId: ExchangeTransaction.Id): AddressPessimisticCorrection =
    if (notObserved.contains(txId)) copy(notObserved = notObserved.removed(txId))
    else
      // Could happen during rollbacks, but CombinedWavesBlockchainClient solves this situation.
      // Even this happen, we just have a hanging txId in "future", which won't affect the process, only consumes small amount of memory.
      copy(future = future + txId)

  /**
   * @return A negative value
   */
  def getBy(asset: Asset): Long = unconfirmed.getOrElse(asset, 0L) - notObserved.valuesIterator
    .flatMap(_.collect { case (`asset`, v) => v })
    .sum

}
