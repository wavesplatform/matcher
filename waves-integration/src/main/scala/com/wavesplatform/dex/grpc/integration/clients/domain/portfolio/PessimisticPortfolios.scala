package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

trait PessimisticPortfolios {
  def getAggregated(address: Address): Map[Asset, Long]

  def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address]
  def addPending(txs: Seq[PessimisticTransaction]): Set[Address]

  /**
   * @return (affected addresses, unknown transactions)
   */
  def processForged(txIds: Seq[ByteString]): (Set[Address], List[ByteString])

  // TODO DEX-1013
  def removeFailed(txIds: Seq[ByteString]): Set[Address]
}
