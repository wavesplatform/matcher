package com.wavesplatform.dex.caches

import com.wavesplatform.dex.settings.OrderFeeSettings.OrderFeeSettings

import scala.collection.immutable.TreeMap

class OrderFeeSettingsCache(orderFeeSettingsMap: Map[Long, OrderFeeSettings]) {

  private val allOrderFeeSettings = {
    if (orderFeeSettingsMap.isEmpty) throw new IllegalArgumentException("Order fee settings should contain at least 1 value!")
    TreeMap.empty[Long, OrderFeeSettings] ++ orderFeeSettingsMap
  }

  def getSettingsForOffset(offset: Long): OrderFeeSettings = {
    allOrderFeeSettings
      .takeWhile { case (o, _) => o <= offset }
      .lastOption
      .map(_._2)
      .getOrElse(throw new IllegalStateException(s"Order fee settings are not set for offset $offset"))
  }
}
