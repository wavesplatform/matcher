package com.wavesplatform.dex.grpc.integration.utx

import cats.implicits.{catsSyntaxOptionId, catsSyntaxSemigroup}
import cats.syntax.option.none
import com.wavesplatform.account.Address
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

/**
 * *
 * Handles utx events, stores current utx transactions + utx diff relevant for specified accounts
 * @param accounts set of addresses for which combined diff will be kept
 * @param accountsDiff combined diff of currently unconfirmed transactions relevant for specified accounts
 * @param transactions list of all transactions which still not confirmed yet
 */
case class UtxState(
  accounts: Set[Address] = Set.empty,
  accountsDiff: Diff = Diff.empty,
  transactions: Map[ByteStr, (UtxTransaction, Diff)] = Map()
) extends ScorexLogging {
  def getAccountsDiff: Diff = accountsDiff

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

  private def relevantDiff(diff: Diff): Boolean =
    diff.transactions.values.exists { txInfo =>
      txInfo.affected.intersect(accounts).nonEmpty
    }

  private def add(tx: Transaction, diff: Diff): (Option[UtxEvent], UtxState) = {
    val utx = UtxTransaction(
      id = tx.id().toPB,
      transaction = tx.toPB.some,
      diff = diff.toPB.some
    )

    val nextTransactions = transactions + (tx.id() -> ((utx, diff)))

    val nextAccountDiff =
      if (relevantDiff(diff))
        accountsDiff |+| diff
      else
        accountsDiff

    val state = copy(
      transactions = nextTransactions,
      accountsDiff = nextAccountDiff
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

      case Some((utx, diff)) =>
        val nextTransactions = transactions - tx.id()

        val nextAccountsDiff =
          if (relevantDiff(diff))
            nextTransactions.foldLeft(Diff.empty) { case (acc, (_, (_, d))) =>
              if (relevantDiff(d))
                acc |+| d
              else
                acc
            }
          else
            accountsDiff

        val state = copy(
          transactions = nextTransactions,
          accountsDiff = nextAccountsDiff
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
