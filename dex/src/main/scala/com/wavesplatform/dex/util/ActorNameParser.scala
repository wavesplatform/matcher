package com.wavesplatform.dex.util

import com.wavesplatform.dex.domain.asset.AssetPair

import scala.util.{Failure, Try}

object ActorNameParser {
  def orderBookPair(orderBookActorName: String): Try[AssetPair] = {
    val name = orderBookActorName
    val xs   = name.split('-')
    if (xs.length == 2) AssetPair.createAssetPair(xs.head, xs(1))
    else Failure(new IllegalArgumentException(s"Can't extract a pair from the order book name: '$orderBookActorName'"))
  }
}
