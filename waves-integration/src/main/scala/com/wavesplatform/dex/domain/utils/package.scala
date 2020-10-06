package com.wavesplatform.dex.domain

import scala.util.{Failure, Success, Try}

package object utils {

  private val BytesMaxValue = 256
  private val Base58MaxValue = 58

  private val BytesLog = math.log(BytesMaxValue)
  private val BaseLog = math.log(Base58MaxValue)

  def base58Length(byteArrayLength: Int): Int = math.ceil(BytesLog / BaseLog * byteArrayLength).toInt

  implicit class EitherExt2[A, B](ei: Either[A, B]) {

    def explicitGet(): B = ei match {
      case Left(value) => throw makeException(value)
      case Right(value) => value
    }

    def foldToTry: Try[B] = ei.fold(
      left => Failure(makeException(left)),
      right => Success(right)
    )

    @inline
    private[this] def makeException(value: Any): Throwable = value match {
      case err: Throwable => err
      case _ => new RuntimeException(value.toString)
    }

  }

}
