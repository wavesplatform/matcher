package com.wavesplatform.transaction.description

import cats.{Functor, Semigroupal}
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{Order, OrderV1, OrderV2, OrderV3}

import scala.util.Try

/**
  * Represents description of the byte entity
  * Field `additionalInfo` can be used for specifying of the repeating byte entities
  */
case class ByteEntityDescription(index: Int, name: String, tpe: String, length: String, subIndex: Int = 0, additionalInfo: String = "")

/**
  * Describes byte representation of the different types. Composition of Byte Entities can be used for deserialization
  * and generation of the documentation of the complex data structures, such as transactions, messages, orders, etc
  */
sealed trait ByteEntity[T] { self =>

  private[description] val ByteType           = "Byte"
  private[description] val BooleanType        = "Boolean"
  private[description] val IntType            = "Int"
  private[description] val LongType           = "Long"
  private[description] val ByteArrayType      = "Array[Byte]"
  private[description] val ByteStrType        = s"ByteStr ($ByteArrayType)"
  private[description] val AddressType        = "Address"
  private[description] val AliasType          = "Alias"
  private[description] val AddressOrAliasType = "Address or Alias"
  private[description] val OrderType          = "Order"
  private[description] val OrderV1Type        = "OrderV1"
  private[description] val ProofsType         = "Proofs"
  private[description] val UnimportantType    = ""

  /** Index of the byte entity. In case of composition of byte entities returns index of the last one */
  val index: Int

  private[description] def generateDoc: Seq[ByteEntityDescription]

  private[description] def deserialize(buf: Array[Byte], offset: Int): Try[(T, Int)]

  def deserializeFromByteArray(buf: Array[Byte]): Try[T] = deserialize(buf, 0) map { case (value, _) => value }

  def map[U](f: T => U): ByteEntity[U] = new ByteEntity[U] {

    val index: Int = self.index

    def generateDoc: Seq[ByteEntityDescription] = self.generateDoc

    def deserialize(buf: Array[Byte], offset: Int): Try[(U, Int)] = self.deserialize(buf, offset).map { case (t, o) => f(t) -> o }
  }

  /** Generates documentation ready for pasting into .md files */
  def getStringDocForMD: String = {
    generateDoc
      .map {
        case ByteEntityDescription(idx, name, tpe, length, subIndex, additionalInfo) =>
          s"| $idx${Option(subIndex).filter(_ != 0).fold("")(si => s".$si")} | $name | $tpe | $length $additionalInfo\n"
            .replace("...", "| ... | ... | ... | ... |")
            .replace("(", "\\(")
            .replace(")", "\\)")
            .replace("*", "\\*")
      }
      .foldLeft("""| \# | Field name | Type | Length in Bytes |""" + "\n| --- | --- | --- | --- |\n")(_ + _)
  }
}

object ByteEntity {

  implicit val byteEntityFunctor: Functor[ByteEntity] = new Functor[ByteEntity] {
    def map[A, B](fa: ByteEntity[A])(f: A => B): ByteEntity[B] = fa map f
  }

  implicit val byteEntitySemigroupal: Semigroupal[ByteEntity] = new Semigroupal[ByteEntity] {
    def product[A, B](fa: ByteEntity[A], fb: ByteEntity[B]): ByteEntity[(A, B)] = Composition(fa, fb)
  }
}

case class ConstantByte(index: Int, value: Byte, name: String) extends ByteEntity[Byte] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, s"$ByteType (constant, value = $value)", "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { value -> (offset + 1) }
  }
}

case class OneByte(index: Int, name: String) extends ByteEntity[Byte] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, ByteType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { buf(offset) -> (offset + 1) }
  }
}

case class LongBytes(index: Int, name: String) extends ByteEntity[Long] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, LongType, "8"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Long, Int)] = {
    Try { Longs.fromByteArray(buf.slice(offset, offset + 8)) -> (offset + 8) }
  }
}

case class IntBytes(index: Int, name: String) extends ByteEntity[Int] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, IntType, "4"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Int, Int)] = {
    Try { Ints.fromByteArray(buf.slice(offset, offset + 4)) -> (offset + 4) }
  }
}

case class SignatureBytes(index: Int, name: String) extends ByteEntity[ByteStr] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, ByteStrType, SignatureLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + SignatureLength)) -> (offset + SignatureLength) }
  }
}

case class ProofsBytes(index: Int, concise: Boolean = true) extends ByteEntity[Proofs] {

  def generateDoc: Seq[ByteEntityDescription] = {
    if (concise) Seq(ByteEntityDescription(index, s"Proofs", ProofsType, "See Proofs structure"))
    else
      Seq(
        ByteEntityDescription(index, s"Proofs version (${Proofs.Version})", UnimportantType, "1", subIndex = 1),
        ByteEntityDescription(index, "Proofs count", UnimportantType, "2", subIndex = 2),
        ByteEntityDescription(index, "Proof 1 length (P1)", UnimportantType, "2", subIndex = 3),
        ByteEntityDescription(index, "Proof 1", ByteStrType, s"P1 <= ${Proofs.MaxProofSize}", subIndex = 4),
        ByteEntityDescription(index, "Proof 2 length (P2)", UnimportantType, "2", subIndex = 5),
        ByteEntityDescription(index, "Proof 2 ", ByteStrType, s"P2 <= ${Proofs.MaxProofSize}", subIndex = 6, additionalInfo = "\n...")
      )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Proofs, Int)] = {
    Try { Proofs.fromBytes(buf.drop(offset)).map(p => p -> (offset + p.bytes.value.length)).explicitGet() }
  }
}

case class OrderBytes(index: Int, name: String) extends ByteEntity[Order] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(index, s"$name size (N)", UnimportantType, "4", subIndex = 1),
      ByteEntityDescription(index, s"$name version mark", UnimportantType, "1 (version 1) / 0 (version 2)", subIndex = 2),
      ByteEntityDescription(index, name, OrderType, "N, see the appropriate Order version structure", subIndex = 3)
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Order, Int)] = {
    Try {

      val orderSize = Ints.fromByteArray(buf.slice(offset, offset + 4))
      val orderMark = buf(offset + 4)

      orderMark match {
        case 1 => OrderV1.parseBytes(buf.drop(offset + 5)).map(order => order -> (offset + 5 + orderSize))
        case 2 => OrderV2.parseBytes(buf.drop(offset + 4)).map(order => order -> (offset + 4 + orderSize))
        case 3 => OrderV3.parseBytes(buf.drop(offset + 4)).map(order => order -> (offset + 4 + orderSize))
      }
    }.flatten
  }
}

case class OrderV1Bytes(index: Int, name: String, length: String) extends ByteEntity[OrderV1] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, OrderV1Type, length))

  def deserialize(buf: Array[Byte], offset: Int): Try[(OrderV1, Int)] = {
    OrderV1.parseBytes(buf.drop(offset)).map { order =>
      order -> (offset + order.bytes.value.length)
    }
  }
}

case class Composition[T1, T2](e1: ByteEntity[T1], e2: ByteEntity[T2]) extends ByteEntity[(T1, T2)] {

  val index: Int = e2.index // use last index in composition

  def generateDoc: Seq[ByteEntityDescription] = e1.generateDoc ++ e2.generateDoc

  def deserialize(buf: Array[Byte], offset: Int): Try[((T1, T2), Int)] =
    for {
      (value1, offset1) <- e1.deserialize(buf, offset)
      (value2, offset2) <- e2.deserialize(buf, offset1)
    } yield ((value1, value2), offset2)
}
