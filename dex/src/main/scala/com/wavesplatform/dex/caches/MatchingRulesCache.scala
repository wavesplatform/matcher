package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import cats.data.NonEmptyList
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair

class MatchingRulesCache(matcherSettings: MatcherSettings, getAssetDecimals: Asset => Int)(implicit errorFormatterContext: ErrorFormatterContext) {

  private val allMatchingRules    = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair): NonEmptyList[DenormalizedMatchingRule] = {
    allMatchingRules.computeIfAbsent(
      assetPair,
      _ => DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, getAssetDecimals, assetPair)
    )
  }

  def getDenormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): DenormalizedMatchingRule = {
    getMatchingRules(assetPair).toList.reverse
      .collectFirst { case mr @ DenormalizedMatchingRule(startOffset, _) if startOffset <= (currentOffset + 1) => mr }
      .getOrElse { DenormalizedMatchingRule.DefaultRule }
  }

  def getNormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): MatchingRule = {
    getDenormalizedRuleForNextOrder(assetPair, currentOffset).normalize(assetPair, getAssetDecimals)
  }

  def updateCurrentMatchingRule(assetPair: AssetPair, denormalizedMatchingRule: DenormalizedMatchingRule): Unit = {
    currentMatchingRule.put(assetPair, denormalizedMatchingRule)
  }

  def setCurrentMatchingRuleForNewOrderBook(assetPair: AssetPair, currentOffset: Long): Unit = {
    updateCurrentMatchingRule(assetPair, getDenormalizedRuleForNextOrder(assetPair, currentOffset))
  }
}
