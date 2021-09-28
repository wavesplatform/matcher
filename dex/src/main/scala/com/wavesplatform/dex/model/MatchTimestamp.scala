package com.wavesplatform.dex.model

object MatchTimestamp {

  def getMatchTimestamp(startOffset: Long)(currentOffset: Long)(orderAddedTs: Long, orderbookTs: Long): Long =
    if (currentOffset < startOffset) orderAddedTs
    else math.max(orderAddedTs, orderbookTs)

}
