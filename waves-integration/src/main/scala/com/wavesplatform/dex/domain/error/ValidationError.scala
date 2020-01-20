package com.wavesplatform.dex.domain.error

import com.wavesplatform.dex.domain.order.Order

import scala.util.Either

trait ValidationError extends Product with Serializable

object ValidationError {

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
}
