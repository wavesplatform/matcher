package com.wavesplatform.dex.domain.transaction

import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.domain.error.ValidationError

final case class ExchangeTransactionResult[A <: ExchangeTransaction](transaction: A, error: Option[ValidationError] = None) {

  def toEither: Either[ValidationError, A] = error.fold(transaction.asRight[ValidationError])(_.asLeft)

  def toOptionTx: Option[A] = toEither.toOption

  def transformTx[B <: ExchangeTransaction](f: A => B): ExchangeTransactionResult[B] =
    copy(transaction = f(transaction))

}

object ExchangeTransactionResult {

  def fromEither[A <: ExchangeTransaction](maybeError: Either[ValidationError, Unit], tx: A): ExchangeTransactionResult[A] =
    maybeError match {
      case Left(value) => ExchangeTransactionResult(tx, value.some)
      case Right(_) => ExchangeTransactionResult(tx, None)
    }

}
