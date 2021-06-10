package com.wavesplatform.dex.tool

import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

final case class OffsetAndTime(offset: Offset, time: Long)
