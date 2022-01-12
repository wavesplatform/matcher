package com.wavesplatform.dex.domain.crypto

import cats.syntax.either._
import com.google.common.primitives.Bytes
import com.wavesplatform.dex.domain.bytes.{deser, ByteStr}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import monix.eval.Coeval

import scala.util.Try

case class Proofs(proofs: List[ByteStr]) {

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(Proofs.Version), deser.serializeArrays(proofs.map(_.arr))))

  def toSignature: ByteStr = proofs.headOption.getOrElse(ByteStr.empty)

  override def toString: String = s"Proofs(${proofs.mkString(", ")})"
}

object Proofs {

  val Version: Byte = 1: Byte
  val MaxProofs: Int = 8
  val MaxProofSize: Int = 64

  lazy val empty = new Proofs(Nil)

  protected def validate(proofs: Seq[ByteStr]): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
      _ <- Either.cond(!proofs.map(_.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
    } yield ()

  def createWithBytes(proofs: Seq[ByteStr], parsedBytes: Array[Byte]): Either[ValidationError, Proofs] =
    validate(proofs) map { _ =>
      new Proofs(proofs.toList) {
        override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
          val proofsLength = 3 + proofs.map(_.length + 2).sum
          if (parsedBytes.length == proofsLength) parsedBytes else parsedBytes.take(proofsLength)
        }
      }
    }

  def create(proofs: Seq[ByteStr]): Either[ValidationError, Proofs] = validate(proofs).map(_ => Proofs(proofs.toList))

  def fromBytes(ab: Array[Byte]): Either[ValidationError, (Proofs, Int)] =
    for {
      _ <- Either.cond(ab.headOption contains 1, (), GenericError(s"Proofs version must be 1, actual:${ab.headOption}"))
      (arrays, length) <- Try(deser parseArrays ab.tail).toEither.leftMap[ValidationError](GenericError.apply)
      proofs <- createWithBytes(arrays.map(ByteStr.apply), ab)
    } yield proofs -> (length + 1)

  implicit def apply(proofs: Seq[ByteStr]): Proofs = new Proofs(proofs.toList)
  implicit def toSeq(proofs: Proofs): Seq[ByteStr] = proofs.proofs
}
