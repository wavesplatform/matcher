package com.wavesplatform.dex.domain.bytes

import com.wavesplatform.dex.domain.bytes.codec.{Base58, Base64}
import play.api.libs.json._

import scala.util.Try

// noinspection ScalaStyle
final case class ByteStr(arr: Array[Byte]) {

  lazy val base58: String = Base58.encode(arr)
  lazy val base64Raw: String = Base64.encode(arr)
  lazy val base64: String = "base64:" + base64Raw

  lazy val trim: String = s"${if (arr.length < 1024) base58.take(7) else base64Raw} ..."

  override lazy val toString: String = if (arr.length < 1024) base58 else base64

  def isEmpty: Boolean = arr.length == 0
  def size: Int = arr.length
  def ++(other: ByteStr): ByteStr = if (this.isEmpty) other else ByteStr(this.arr ++ other.arr)

  def take(n: Long): ByteStr = {
    val n1 = n min arr.length max 0

    if (n1 == arr.length) this
    else if (n1 == 0) ByteStr.empty
    else ByteStr(arr.take(n1.toInt))
  }

  def drop(n: Long): ByteStr = {
    val n1 = n min arr.length max 0

    if (n1 == arr.length) ByteStr.empty
    else if (n1 == 0) this
    else ByteStr(arr.drop(n1.toInt))
  }

  def takeRight(n: Long): ByteStr = drop(arr.length.toLong - n)
  def dropRight(n: Long): ByteStr = take(arr.length.toLong - n.max(0))

  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => java.util.Arrays.equals(arr, other.arr)
    case _ => false
  }

  override lazy val hashCode: Int = java.util.Arrays.hashCode(arr)
}

object ByteStr {

  val empty: ByteStr = ByteStr(Array.emptyByteArray)

  implicit def fromByteArray(arr: Array[Byte]): ByteStr = new ByteStr(arr)
  implicit def toByteArray(bs: ByteStr): Array[Byte] = bs.arr

  def decodeBase58(s: String): Try[ByteStr] = Base58.tryDecodeWithLimit(s).map(ByteStr(_))
  def decodeBase64(s: String): Try[ByteStr] = Base64.tryDecode(s).map(ByteStr(_))

  // scorex.utils.ByteArray.compare
  private[this] def compare(buffer1: Array[Byte], buffer2: Array[Byte]): Int =
    if (buffer1 sameElements buffer2) 0
    else {

      val end1: Int = if (buffer1.length < buffer2.length) buffer1.length else buffer2.length
      var i: Int = 0

      while (i < end1) {
        val a: Int = buffer1(i) & 0xff
        val b: Int = buffer2(i) & 0xff
        if (a != b)
          return a - b
        i = i + 1
      }
      buffer1.length - buffer2.length
    }

  implicit val byteStrOrdering: Ordering[ByteStr] = (x, y) => compare(x.arr, y.arr)

  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), JsSuccess(_))
      case _ => JsError("Expected JsString")
    },
    Writes(b => JsString(b.base58))
  )

}
