package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import cats.data.NonEmptyList
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.AssetPair

class MatchingRulesCache(matcherSettings: MatcherSettings, blockchain: Blockchain)(implicit errorFormatterContext: ErrorFormatterContext) {

  private val allMatchingRules    = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair): NonEmptyList[DenormalizedMatchingRule] = {
    allMatchingRules.getOrDefault(assetPair, DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, blockchain, assetPair))
  }

  def getDenormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): DenormalizedMatchingRule = {
    getMatchingRules(assetPair)
      .find { _.startOffset == currentOffset + 1 }
      .getOrElse { currentMatchingRule.get(assetPair) }
  }

  def getNormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): MatchingRule = {
    getDenormalizedRuleForNextOrder(assetPair, currentOffset).normalize(assetPair, blockchain)
  }

  def updateCurrentMatchingRule(assetPair: AssetPair, matchingRule: DenormalizedMatchingRule): Unit = {
    currentMatchingRule.put(assetPair, matchingRule)
  }

  def setCurrentMatchingRuleForNewOrderBook(assetPair: AssetPair): Unit = {
    updateCurrentMatchingRule(assetPair, getMatchingRules(assetPair).head)
  }
}
