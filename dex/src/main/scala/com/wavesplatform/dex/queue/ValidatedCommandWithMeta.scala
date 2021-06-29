package com.wavesplatform.dex.queue

import com.google.common.primitives.Longs

case class ValidatedCommandWithMeta(offset: ValidatedCommandWithMeta.Offset, timestamp: Long, command: ValidatedCommand) {

  override def toString: String =
    s"ValidatedCommandWithMeta(offset=$offset, ts=$timestamp, ${command.toString})"

}

object ValidatedCommandWithMeta {

  type Offset = Long

  def toBytes(x: ValidatedCommandWithMeta): Array[Byte] =
    Longs.toByteArray(x.offset) ++ Longs.toByteArray(x.timestamp) ++ ValidatedCommand.toBytes(x.command)

  def fromBytes(xs: Array[Byte]): ValidatedCommandWithMeta = ValidatedCommandWithMeta(
    offset = Longs.fromByteArray(xs.take(8)),
    timestamp = Longs.fromByteArray(xs.slice(8, 16)),
    command = ValidatedCommand.fromBytes(xs.drop(16))
  )

}
