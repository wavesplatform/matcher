package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import cats.data.NonEmptyList
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair

class MatchingRulesCache(matcherSettings: MatcherSettings) {

  private val allMatchingRules    = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair, assetDecimals: Asset => Int): NonEmptyList[DenormalizedMatchingRule] = {
    allMatchingRules.computeIfAbsent(
      assetPair,
      _ => DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, assetDecimals, assetPair)
    )
  }

  def getDenormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long, assetDecimals: Asset => Int): DenormalizedMatchingRule = {
    getMatchingRules(assetPair, assetDecimals).toList.reverse
      .collectFirst { case mr @ DenormalizedMatchingRule(startOffset, _) if startOffset <= (currentOffset + 1) => mr }
      .getOrElse { DenormalizedMatchingRule.getDefaultRule(assetPair, assetDecimals) }
  }

  def getNormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long, assetDecimals: Asset => Int): MatchingRule = {
    getDenormalizedRuleForNextOrder(assetPair, currentOffset, assetDecimals).normalize(assetPair, assetDecimals)
  }

  def updateCurrentMatchingRule(assetPair: AssetPair, denormalizedMatchingRule: DenormalizedMatchingRule): Unit = {
    currentMatchingRule.put(assetPair, denormalizedMatchingRule)
  }

  def setCurrentMatchingRuleForNewOrderBook(assetPair: AssetPair, currentOffset: Long, assetDecimals: Asset => Int): Unit = {
    updateCurrentMatchingRule(assetPair, getDenormalizedRuleForNextOrder(assetPair, currentOffset, assetDecimals))
  }
}
