package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.TxValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

import java.io.{PrintWriter, StringWriter}
import scala.annotation.tailrec

package object error {

  def formatStackTrace(e: Throwable): String = {
    val stringWriter = new StringWriter()
    e.printStackTrace(new PrintWriter(stringWriter))
    stringWriter.flush()
    stringWriter.getBuffer.toString
  }

  @tailrec
  def canRetry(x: ValidationError, tx: Option[ExchangeTransaction], lpAccounts: Set[Address]): Boolean = x match {
    case x: GenericError
        if x.err == "Transaction pool bytes size limit is reached"
          || x.err == "Transaction pool size limit is reached"
          || x.err.contains("There are not enough connections with peers") => true

    // Could happen when:
    // 1. One transaction is sent multiple times in parallel
    // 2. There are two exchanges tx1 and tx2 those fill the order:
    // 2.1. tx1 is mined and still present in UTX pool (race condition on NODE), thus the order considered as partially filled by tx1 * 2
    // 2.2. tx2 fails for some reason

    //error message was taken from:
    //https://github.com/wavesplatform/Waves/blob/master/node/src/main/scala/com/wavesplatform/state/diffs/ExchangeTransactionDiff.scala#L169-L171
    case x: TxValidationError.OrderValidationError if x.err.contains("Too much") => true

    //error message was taken from:
    //https://github.com/wavesplatform/Waves/blob/master/node/src/main/scala/com/wavesplatform/state/diffs/BalanceDiffValidation.scala#L49
    case TxValidationError.AccountBalanceError(errs) if errs.values.exists(_.contains("negative asset balance")) => true

    //fix for LP
    case x: TxValidationError.ScriptExecutionError
        if x.message.contains("order validation failed") &&
          tx.map(_.order1.senderAddress).exists(lpAccounts.contains) ||
          tx.map(_.order2.senderAddress).exists(lpAccounts.contains) => true

    case TransactionValidationError(cause, _) => canRetry(cause, tx, lpAccounts)

    case _ => false
  }

}
