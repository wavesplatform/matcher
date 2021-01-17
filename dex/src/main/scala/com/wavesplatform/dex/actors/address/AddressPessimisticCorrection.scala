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
 * @param futureTxs ExchangeTransactions which hasn't been registered as unconfirmed, but will be
 */
case class AddressPessimisticCorrection(
  notObserved: Map[ExchangeTransaction.Id, Map[Asset, Long]],
  unconfirmed: Map[Asset, Long],
  futureTxs: Set[ExchangeTransaction.Id]
) {
  // properties:
  // base always negative
  // either in unconfirmed, or futureExchangeTxs

  def withProbablyStaleUnconfirmed(update: Map[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = update ++ unconfirmed)

  def withFreshUnconfirmed(update: Map[Asset, Long]): AddressPessimisticCorrection = copy(unconfirmed = unconfirmed ++ update)

  def withExecuted(txId: ExchangeTransaction.Id, volume: Map[Asset, Long]): AddressPessimisticCorrection =
    if (futureTxs.contains(txId))
      copy(
        unconfirmed = unconfirmed |-| volume,
        futureTxs = futureTxs - txId
      )
    else copy(
      unconfirmed = unconfirmed |-| volume,
      notObserved = notObserved.updated(txId, volume)
    )

  def withObserved(txId: ExchangeTransaction.Id): AddressPessimisticCorrection =
    if (notObserved.contains(txId)) copy(notObserved = notObserved.removed(txId))
    else
      // Should not happen, because a transaction shouldn't be forged and registered in CombinedWavesBlockchainClient twice.
      // Even this happen, we just have a hanging txId in futureTxs, which won't affect the process, only consumes small amount of memory.
      copy(futureTxs = futureTxs + txId)

  /**
   * @return A negative value
   */
  def getBy(asset: Asset): Long = unconfirmed.getOrElse(asset, 0L) - notObserved.valuesIterator
    .flatMap(_.collect { case (`asset`, v) => v })
    .sum

}
