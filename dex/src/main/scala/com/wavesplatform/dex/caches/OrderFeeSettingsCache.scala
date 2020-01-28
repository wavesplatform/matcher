package com.wavesplatform.dex.caches

import com.wavesplatform.dex.settings.OrderFeeSettings.OrderFeeSettings

import scala.collection.immutable.TreeMap

class OrderFeeSettingsCache(orderFeeSettingsMap: Map[Long, OrderFeeSettings], currentOffset: => Long) {

  private val allOrderFeeSettings = {
    if (!orderFeeSettingsMap.contains(0)) throw new IllegalArgumentException("Order fee settings should contain value for 0 offset!")
    TreeMap.empty[Long, OrderFeeSettings] ++ orderFeeSettingsMap
  }

  private def getClosestActualSettings(offset: Long): OrderFeeSettings = {
    allOrderFeeSettings.toSeq.reverse
      .collectFirst { case (startOffset, settings) if startOffset <= offset => settings }
      .getOrElse(allOrderFeeSettings.head._2)
  }

  def getCurrentFeeSettings: OrderFeeSettings      = getClosestActualSettings(currentOffset)
  def getFeeSettingsForNextOrder: OrderFeeSettings = getClosestActualSettings(currentOffset + 1)
}
