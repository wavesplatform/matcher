package com.wavesplatform.dex.grpc.integration.utx

import cats.implicits.catsSyntaxOptionId
import cats.syntax.option.none
import com.wavesplatform.account.Address
import com.wavesplatform.api.grpc._
import com.wavesplatform.dex.grpc.integration.error._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions.{VanillaByteStrOps, _}
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, UtxTransaction}
import com.wavesplatform.events
import com.wavesplatform.events.UtxEvent.{TxAdded, TxRemoved}
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.ScorexLogging

case class UtxState(
  accounts: Set[Address] = Set.empty,
  queue: UtxQueue = UtxQueue(),
  diff: UtxDiff = UtxDiff()
) extends ScorexLogging {
  def getAccountsDiff: Diff = diff.accountsDiff

  def getUtxTransactions: List[UtxTransaction] = diff.transactions.values.map(_._1).toList

  def handleEvent(evt: events.UtxEvent): (List[UtxAction], UtxState) =
    evt match {
      case TxRemoved(tx, Some(reason)) if canRetry(reason) =>
        log.debug(s"${tx.id()} failed by a false-positive reason: $reason")
        (Nil, this)

      case TxRemoved(tx, reason) =>
        val (maybeRemoved, nextDiff) = diff.removeTx(tx)
        val maybeEvent = maybeRemoved.map { utx =>
          UtxEvent(UtxEvent.Type.Update(UtxEvent.Update(
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
        }

        (maybeEvent.map(Notify).toList, copy(diff = nextDiff))

      case TxAdded(tx, d) =>
        val utx = UtxTransaction(
          id = tx.id().toPB,
          transaction = tx.toPB.some,
          diff = d.toPB.some
        )

        val (nextDiff, maybeTx, nextQueue) =
          if (relevantDiff(d)) {
            val nextDiff = diff.addTx(tx, d)
            val (maybeTx, nextQueue) = queue.onTxAdded(tx)

            (nextDiff, maybeTx, nextQueue)
          } else
            (diff, none, queue)

        val event = UtxEvent(
          UtxEvent.Type.Update(
            UtxEvent.Update(
              added = List(UtxEvent.Update.Added(utx.some))
            )
          )
        )

        (Notify(event) :: maybeTx.map(Broadcast).toList, copy(queue = nextQueue, diff = nextDiff))
    }

  def putInQueueIfRelevantOrReturn(tx: ExchangeTransaction): (Option[ExchangeTransaction], UtxState) =
    if (relevantTx(tx)) {
      val (maybeTx, nextQueue) = queue.enqueue(tx)
      (maybeTx, copy(queue = nextQueue))
    } else
      (tx.some, this)

  private def relevantDiff(diff: Diff): Boolean =
    diff.transactions.values.exists { txInfo =>
      txInfo.affected.intersect(accounts).nonEmpty
    }

  private def relevantTx(tx: ExchangeTransaction): Boolean =
    tx match {
      case t: ExchangeTransaction =>
        accounts(t.buyOrder.sender.toAddress(t.chainId)) || accounts(t.sellOrder.sender.toAddress(t.chainId))

      case _ => false
    }

  private def getSimpleName(x: Any): String = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")
}
