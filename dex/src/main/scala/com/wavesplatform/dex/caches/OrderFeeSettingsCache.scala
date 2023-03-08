package com.wavesplatform.dex.caches

import com.wavesplatform.dex.caches.OrderFeeSettingsCache.{AssetsActionForOffset, CustomAssetFeeState}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.OrderFeeSettings

import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.TreeMap

class OrderFeeSettingsCache(
  orderFeeSettingsMap: Map[Offset, OrderFeeSettings],
  initialAssetsState: CustomAssetFeeState = CustomAssetFeeState.empty
) extends ScorexLogging {

  log.debug(s"Initial assets state $initialAssetsState")

  private val customAssetsState = new AtomicReference[CustomAssetFeeState](initialAssetsState)

  private val allOrderFeeSettings = {
    if (orderFeeSettingsMap.isEmpty) throw new IllegalArgumentException("Order fee settings should contain at least 1 value!")
    TreeMap.empty[Offset, OrderFeeSettings] ++ orderFeeSettingsMap
  }

  def applyAssetsActionForOffset(actionForOffset: AssetsActionForOffset): Unit = {
    log.info(s"Applying $actionForOffset")
    customAssetsState.updateAndGet(s => s.applyAssetsActionForOffset(actionForOffset))
  }

  def containsCustomFeeAsset(asset: Asset): Boolean = customAssetsState.get().getForLatestOffset().contains(asset)

  def getSettingsForOffset(offset: Offset): OrderFeeSettings =
    allOrderFeeSettings
      .takeWhile { case (o, _) => o <= offset }
      .lastOption
      .map(_._2.withDynamicCustomAssets(customAssetsState.get().getAssetsForOffset(offset)))
      .getOrElse(throw new IllegalStateException(s"Order fee settings are not set for offset $offset"))

}

object OrderFeeSettingsCache {

  final case class AssetsActionForOffset(offset: Offset, assets: Set[Asset], isAdded: Boolean)

  final case class CustomAssetFeeState(assetFeeState: Map[Offset, Set[Asset]]) {

    private val assetsMapKeys = assetFeeState.keySet.toSeq.sorted

    val latestAssetOffsetOpt: Option[Offset] = assetsMapKeys.lastOption

    def getAssetsForOffset(offset: Offset): Set[Asset] =
      assetsMapKeys
        .takeWhile(_ < offset)
        .lastOption
        .flatMap(assetFeeState.get)
        .getOrElse(Set.empty) // because it means there is nothing in map

    def applyAssetsActionForOffset(actionForOffset: AssetsActionForOffset): CustomAssetFeeState =
      if (actionForOffset.isAdded) addAssets(actionForOffset.offset, actionForOffset.assets)
      else removeAssets(actionForOffset.offset, actionForOffset.assets)

    def addAssets(offset: Offset, assets: Set[Asset]): CustomAssetFeeState =
      latestAssetOffsetOpt.fold(CustomAssetFeeState(Map(offset -> assets))) { lastOffset =>
        val prevState = assetFeeState.getOrElse(lastOffset, Set.empty)
        val newAssets = prevState ++ assets
        CustomAssetFeeState(assetFeeState + (offset -> newAssets))
      }

    def removeAssets(offset: Offset, assets: Set[Asset]): CustomAssetFeeState =
      latestAssetOffsetOpt.fold(CustomAssetFeeState(Map.empty)) { lastOffset =>
        assetFeeState.get(lastOffset) match {
          case Some(prevState) =>
            val newAssets = prevState -- assets
            CustomAssetFeeState(assetFeeState + (offset -> newAssets))
          case None =>
            CustomAssetFeeState(Map.empty)
        }
      }

    def getForLatestOffset(): Set[Asset] =
      latestAssetOffsetOpt.flatMap(assetFeeState.get).getOrElse(Set.empty)

  }

  object CustomAssetFeeState {

    val empty: CustomAssetFeeState = CustomAssetFeeState(Map.empty[Offset, Set[Asset]])

  }

}
