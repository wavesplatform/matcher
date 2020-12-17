package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

case class UtxUpdate(
  unconfirmedTxs: Seq[UtxTransaction] = Nil,
  forgedTxIds: Set[ByteString] = Set.empty,
  failedTxIds: Set[ByteString] = Set.empty,
  resetCaches: Boolean = false
) {
  override def toString: String = s"UtxUpdate(u=${unconfirmedTxs.size}, fgtx=${forgedTxIds.size}, fltx=${failedTxIds.size}, r=$resetCaches)"
}

object UtxUpdate {

  implicit val utxUpdateMonoid: Monoid[UtxUpdate] = new Monoid[UtxUpdate] {
    override val empty: UtxUpdate = UtxUpdate()

    // TODO DEX-1002 resetCaches optimization: only unconfirmedTxs
    override def combine(x: UtxUpdate, y: UtxUpdate): UtxUpdate = UtxUpdate(
      unconfirmedTxs = removeDone(x.unconfirmedTxs, y) ++ removeDone(y.unconfirmedTxs, x),
      forgedTxIds = x.forgedTxIds.union(y.forgedTxIds),
      failedTxIds = x.failedTxIds.union(y.failedTxIds),
      resetCaches = x.resetCaches || y.resetCaches
    )

    private def removeDone(
      unconfirmedTxs: Seq[UtxTransaction],
      by: UtxUpdate
    ): Seq[UtxTransaction] = unconfirmedTxs.filterNot(tx => by.forgedTxIds.contains(tx.id) || by.failedTxIds.contains(tx.id))

  }

}
