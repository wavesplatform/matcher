package com.wavesplatform.dex.domain.bytes

import com.google.common.primitives.{Bytes, Shorts}

package object deser {

  def serializeArray(b: Array[Byte]): Array[Byte] = {
    val length = b.length
    if (length.isValidShort) Bytes.concat(Shorts.toByteArray(length.toShort), b)
    else throw new IllegalArgumentException(s"Attempting to serialize array with size, but the size($length) exceeds MaxShort(${Short.MaxValue})")
  }

  def parseArrayWithLength(bytes: Array[Byte], position: Int): (Array[Byte], Int) = {
    val from = position + 2
    val length = Shorts.fromByteArray(bytes.slice(position, from))
    val to = from + length
    require(length >= 0, s"Array length should be non-negative, but $length found")
    require(bytes.length >= to, s"Array length = ${bytes.length} less than slice end point index = $to")
    (bytes.slice(from, to), to)
  }

  def parseByteArrayOption(bytes: Array[Byte], position: Int, length: Int): (Option[Array[Byte]], Int) =
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val b = bytes.slice(position + 1, position + 1 + length)
      (Some(b), position + 1 + length)
    } else (None, position + 1)

  def parseArrays(bytes: Array[Byte]): (Seq[Array[Byte]], Int) = {

    val arraysCount = Shorts.fromByteArray(bytes.slice(0, 2))

    require(arraysCount >= 0, s"Arrays count should be non-negative, but $arraysCount found")
    require(arraysCount <= (bytes.length - 2) / 2, s"Bytes with length = ${bytes.length - 2} can't contain $arraysCount array(s)")

    (0 until arraysCount).foldLeft((Seq.empty[Array[Byte]], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArrayWithLength(bytes, pos)
        (acc :+ arr, nextPos)
    }
  }

  def serializeArrays(bs: Seq[Array[Byte]]): Array[Byte] = {
    require(bs.length.isValidShort, s"Attempting to serialize array with size, but the size(${bs.length}) exceeds MaxShort(${Short.MaxValue})")
    val countBytes = Shorts.toByteArray(bs.length.toShort)
    Bytes.concat(Seq(countBytes) ++ bs.map(serializeArray): _*)
  }

}
