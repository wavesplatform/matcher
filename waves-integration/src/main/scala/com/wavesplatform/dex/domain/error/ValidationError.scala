package com.wavesplatform.dex.domain.error

import com.wavesplatform.dex.domain.order.Order

import scala.util.Either

trait ValidationError extends Product with Serializable {
  def message: String
}

object ValidationError {

  type Validation[T] = Either[ValidationError, T]

  case class InvalidAsset(asset: String, reason: String) extends ValidationError {
    override def message: String = s"Invalid asset $asset, reason $reason"
  }

  case class InvalidBase58String(reason: String) extends ValidationError {
    override def message: String = s"Invalid base 58 string $reason"
  }

  case class InvalidAddress(reason: String) extends ValidationError {
    override def message: String = s"Invalid address $reason"
  }

  case class InvalidPublicKey(reason: String) extends ValidationError {
    override def message: String = s"Invalid public key $reason"
  }

  case class NonPositiveAmount(amount: Long, of: String) extends ValidationError {
    override def message: String = s"Non positive amount $amount of $of"
  }

  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError {
    override def message: String = s"Insufficient fee $msg"
  }

  case class OrderValidationError(order: Order, err: String) extends ValidationError {
    override def message: String = s"Order validation error for order $order, err $err"
  }

  case class ActivationError(err: String) extends ValidationError {
    override def message: String = s"Activation error $err"
  }

  case class GenericError(err: String) extends ValidationError {
    override def message: String = s"Generic error $err"
  }

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(ex.getMessage)
  }

}
