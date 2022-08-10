package com.wavesplatform.dex.caches

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.settings.OrderFeeSettings

import scala.collection.immutable.TreeMap

class OrderFeeSettingsCache(orderFeeRawSettingsMap: Map[Long, OrderFeeSettings], pairValidator: AssetPair => Boolean) {

  private val orderFeeSettingsMap = orderFeeRawSettingsMap.map {
    case (offset, settings: OrderFeeSettings.CompositeSettings) =>
      (offset, settings.filterPairs(pairValidator))
    case (offset, settings) => (offset, settings)
  }

  private val allOrderFeeSettings = {
    if (orderFeeSettingsMap.isEmpty) throw new IllegalArgumentException("Order fee settings should contain at least 1 value!")
    TreeMap.empty[Long, OrderFeeSettings] ++ orderFeeSettingsMap
  }

  def getSettingsForOffset(offset: Long): OrderFeeSettings =
    allOrderFeeSettings
      .takeWhile { case (o, _) => o <= offset }
      .lastOption
      .map(_._2)
      .getOrElse(throw new IllegalStateException(s"Order fee settings are not set for offset $offset"))

}
