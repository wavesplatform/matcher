package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

case class UtxUpdate(
  unconfirmedTxs: Seq[UtxTransaction] = Nil,
  confirmedTxs: Map[ByteString, TransactionWithChanges] = Map.empty,
  failedTxs: Map[ByteString, UtxTransaction] = Map.empty,
  resetCaches: Boolean = false
) {
  override def toString: String = s"UtxUpdate(u=${unconfirmedTxs.size}, fgtx=${confirmedTxs.size}, fltx=${failedTxs.size}, r=$resetCaches)"
}

object UtxUpdate {

  implicit val utxUpdateMonoid: Monoid[UtxUpdate] = new Monoid[UtxUpdate] {
    override val empty: UtxUpdate = UtxUpdate()

    // TODO DEX-1002 resetCaches optimization: only unconfirmedTxs
    override def combine(x: UtxUpdate, y: UtxUpdate): UtxUpdate = UtxUpdate(
      unconfirmedTxs = removeDone(x.unconfirmedTxs, y) ++ removeDone(y.unconfirmedTxs, x),
      confirmedTxs = x.confirmedTxs ++ y.confirmedTxs,
      failedTxs = x.failedTxs ++ y.failedTxs,
      resetCaches = x.resetCaches || y.resetCaches
    )

    private def removeDone(
      unconfirmedTxs: Seq[UtxTransaction],
      by: UtxUpdate
    ): Seq[UtxTransaction] = unconfirmedTxs.filterNot(tx => by.confirmedTxs.contains(tx.id) || by.failedTxs.contains(tx.id))

  }

}
