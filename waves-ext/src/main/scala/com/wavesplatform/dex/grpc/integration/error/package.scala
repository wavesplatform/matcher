package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.TxValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

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
  def canRetry(x: ValidationError): Boolean = x match {
    case x: GenericError
        if x.err == "Transaction pool bytes size limit is reached"
          || x.err == "Transaction pool size limit is reached"
          || x.err.startsWith("There are not enough connections with peers") => true

    //error message was taken from:
    //https://github.com/wavesplatform/Waves/blob/master/node/src/main/scala/com/wavesplatform/state/diffs/BalanceDiffValidation.scala#L49
    case TxValidationError.AccountBalanceError(errs) if errs.values.exists(_.startsWith("negative asset balance")) => true

    case TransactionValidationError(cause, _) => canRetry(cause)

    case _ => false
  }

}
