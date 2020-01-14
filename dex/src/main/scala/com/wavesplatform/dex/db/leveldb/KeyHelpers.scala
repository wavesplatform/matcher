package com.wavesplatform.dex.db.leveldb

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs, Shorts}

object KeyHelpers {

  def hBytes(prefix: Short, height: Int, bytes: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).putInt(height).put(bytes).array()

  def bytes(prefix: Short, bytes: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  def intKey(name: String, prefix: Short, default: Int = 0): Key[Int] =
    Key(name, Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def longKey(name: String, prefix: Short, default: Long = 0): Key[Long] =
    Key(name, Longs.toByteArray(prefix), Option(_).fold(default)(Longs.fromByteArray), Longs.toByteArray)

  def bytesSeqNr(name: String, prefix: Short, b: Array[Byte], default: Int = 0): Key[Int] =
    Key(name, bytes(prefix, b), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)
}
