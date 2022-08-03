package com.wavesplatform.dex

import cats.syntax.applicativeError._
import com.google.protobuf.CodedOutputStream
import com.wavesplatform.dex.domain.bytes.ByteStr
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import scala.util.control.NonFatal

object PBUtils {

  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val outArray = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(outArray)

    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)

    try outputStream.checkNoSpaceLeft()
    catch {
      case NonFatal(e) =>
        throw new RuntimeException(s"Error serializing PB message: $msg (bytes = ${ByteStr(msg.toByteArray)})", e)
    }

    outArray
  }

  def decode[A <: GeneratedMessage](msg: Array[Byte], cmp: GeneratedMessageCompanion[A]): Either[Throwable, A] =
    cmp
      .validate(msg)
      .toEither
      .adaptErr {
        case err => new RuntimeException(s"Error deserializing PB message: $cmp (bytes = ${ByteStr(msg)})", err)
      }

}
