package com.wavesplatform.dex.grpc.integration.utx

import cats.implicits.{catsSyntaxOptionId, catsSyntaxSemigroup}
import cats.syntax.option.none
import com.wavesplatform.api.grpc._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.error._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions.{VanillaByteStrOps, _}
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, UtxTransaction}
import com.wavesplatform.events
import com.wavesplatform.events.UtxEvent.{TxAdded, TxRemoved}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging

case class UtxState(transactions: Map[ByteStr, (UtxTransaction, Diff)] = Map(), combinedDiff: Diff = Diff.empty) extends ScorexLogging {
  def getCombinedDiff: Diff = combinedDiff

  def getUtxTransactions: List[UtxTransaction] = transactions.values.map(_._1).toList

  def handleEvent(evt: events.UtxEvent): (Option[UtxEvent], UtxState) =
    evt match {
      case TxRemoved(tx, Some(reason)) if canRetry(reason) =>
        log.debug(s"${tx.id()} failed by a false-positive reason: $reason")
        (none, this)
      case TxRemoved(tx, reason) => remove(tx, reason)
      case TxAdded(tx, diff) => add(tx, diff)
    }

  private def getSimpleName(x: Any): String = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")

  private def add(tx: Transaction, diff: Diff): (Option[UtxEvent], UtxState) = {
    val utx = UtxTransaction(
      id = tx.id().toPB,
      transaction = tx.toPB.some,
      diff = diff.toPB.some
    )

    val state = copy(
      transactions = transactions + (tx.id() -> ((utx, diff))),
      combinedDiff = combinedDiff |+| diff
    )

    val event = UtxEvent(
      UtxEvent.Type.Update(
        UtxEvent.Update(
          added = List(UtxEvent.Update.Added(utx.some))
        )
      )
    )

    (event.some, state)
  }

  private def remove(tx: Transaction, reason: Option[ValidationError]): (Option[UtxEvent], UtxState) =
    transactions.get(tx.id()) match {
      case None =>
        log.debug(s"Can't find removed ${tx.id()} with reason: $reason")
        (none, this)

      case Some((utx, _)) =>
        val txs = transactions - tx.id()
        val diff = txs.foldLeft(Diff.empty) { case (acc, (_, (_, d))) => acc |+| d }

        val state = copy(
          transactions = txs,
          combinedDiff = diff
        )

        val event = UtxEvent(UtxEvent.Type.Update(UtxEvent.Update(
          removed = List(
            UtxEvent.Update.Removed(
              utx.some,
              reason.map(r =>
                UtxEvent.Update.Removed.Reason(
                  name = getSimpleName(r),
                  message = r.toString
                )
              )
            )
          )
        )))

        (event.some, state)
    }

}
