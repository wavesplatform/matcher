package com.wavesplatform.dex.domain.transaction

import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.domain.error.ValidationError

// contains transaction and possible validation error
final case class ExchangeTransactionResult[A <: ExchangeTransaction](transaction: A, error: Option[ValidationError] = None) {

  // returns Right only if there is no error
  def toEither: Either[ValidationError, A] = error.fold(transaction.asRight[ValidationError])(_.asLeft)

  // returns Some only if there is no error
  def toOption: Option[A] = error.fold(transaction.some)(_ => None)

  // works always, because we always have a transaction
  def map[B <: ExchangeTransaction](f: A => B): ExchangeTransactionResult[B] =
    copy(transaction = f(transaction))

}

object ExchangeTransactionResult {

  def fromEither[A <: ExchangeTransaction](maybeError: Either[ValidationError, Unit], tx: A): ExchangeTransactionResult[A] =
    ExchangeTransactionResult(tx, maybeError.left.toOption)

}
