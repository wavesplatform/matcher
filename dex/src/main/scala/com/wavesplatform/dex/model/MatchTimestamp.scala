package com.wavesplatform.dex.model

object MatchTimestamp {

  def getMatchTimestamp(startOffset: Long)(currentOffset: Long)(takerOrderAddedTs: Long, orderBookTs: Long): Long =
    if (currentOffset < startOffset) takerOrderAddedTs
    else math.max(takerOrderAddedTs, orderBookTs)

}
