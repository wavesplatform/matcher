package com.wavesplatform.transaction

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.exchange.Order

import scala.util.Either

object TxValidationError {

  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String)                    extends ValidationError
  case class NonPositiveAmount(amount: Long, of: String)       extends ValidationError
  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError
  case class OrderValidationError(order: Order, err: String)   extends ValidationError
  case class ActivationError(err: String)                      extends ValidationError
  case class GenericError(err: String)                         extends ValidationError

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(ex.getMessage)
  }

  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }

  trait HasScriptType extends ValidationError {
    def isAssetScript: Boolean
  }
}
