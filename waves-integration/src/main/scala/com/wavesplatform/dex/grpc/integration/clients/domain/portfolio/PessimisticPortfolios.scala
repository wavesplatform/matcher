package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.syntax.monoid._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

// TODO DEX-1013
trait PessimisticPortfolios {
  def getAggregated(address: Address): Map[Asset, Long]

  def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address]
  def addPending(txs: Iterable[PessimisticTransaction]): Set[Address]

  /**
   * @return (affected addresses, unknown transactions)
   */
  def processConfirmed(txIds: Iterable[ByteString]): (Set[Address], List[ByteString])

  // Similar to processConfirmed, but we ignore unknown transactions
  def removeFailed(txIds: Iterable[ByteString]): Set[Address]

  // TODO DEX-1013
  def processUtxUpdate(
    unconfirmedTxs: Seq[PessimisticTransaction],
    confirmedTxs: Seq[ByteString],
    failedTxs: Seq[PessimisticTransaction],
    resetCaches: Boolean
  ): Set[Address] = {
    val addedOrReplaced =
      if (resetCaches)
        replaceWith(unconfirmedTxs)
      else
        addPending(unconfirmedTxs)

    addedOrReplaced |+|
    processConfirmed(confirmedTxs)._1 |+|
    removeFailed(failedTxs.map(_.txId))
  }

}
