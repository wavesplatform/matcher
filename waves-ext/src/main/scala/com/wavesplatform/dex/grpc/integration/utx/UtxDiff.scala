package com.wavesplatform.dex.grpc.integration.utx

import cats.implicits.{catsSyntaxOptionId, catsSyntaxSemigroup}
import cats.syntax.option.none
import com.wavesplatform.api.grpc._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions.{VanillaByteStrOps, _}
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging

// todo: docs/comment
case class UtxDiff(
  accountsDiff: Diff = Diff.empty,
  transactions: Map[ByteStr, (UtxTransaction, Diff)] = Map()
) extends ScorexLogging {

  def addTx(tx: Transaction, diff: Diff): UtxDiff = {
    val utx = UtxTransaction(
      id = tx.id().toPB,
      transaction = tx.toPB.some,
      diff = diff.toPB.some
    )

    val nextTransactions = transactions + (tx.id() -> ((utx, diff)))

    val nextAccountDiff = accountsDiff |+| diff

    val state = copy(
      transactions = nextTransactions,
      accountsDiff = nextAccountDiff
    )

    state
  }

  def removeTx(tx: Transaction): (Option[UtxTransaction], UtxDiff) =
    transactions.get(tx.id()).fold((none[UtxTransaction], this)) { case (utx, _) =>
      val nextTransactions = transactions - tx.id()

      val nextAccountsDiff = nextTransactions.foldLeft(Diff.empty) { case (acc, (_, (_, d))) =>
        acc |+| d
      }

      val state = copy(
        transactions = nextTransactions,
        accountsDiff = nextAccountsDiff
      )

      (utx.some, state)
    }

}
