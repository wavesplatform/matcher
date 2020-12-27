package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

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
}
