package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import cats.data.NonEmptyList
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging

import scala.util.Try
import scala.util.control.NonFatal

class MatchingRulesCache(matcherSettings: MatcherSettings, blockchain: Blockchain)(implicit errorFormatterContext: ErrorFormatterContext)
    extends ScorexLogging {

  private val allMatchingRules    = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair): NonEmptyList[DenormalizedMatchingRule] = {
    allMatchingRules.computeIfAbsent(assetPair, _ => DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, blockchain, assetPair))
  }

  // DEX-488 TODO remove after found a reason of NPE
  def getDenormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): DenormalizedMatchingRule = {
    val result =
      Try {
        getMatchingRules(assetPair).toList.reverse.collectFirst {
          case mr @ DenormalizedMatchingRule(startOffset, _) if startOffset <= (currentOffset + 1) => mr
        }
      }.recover { case NonFatal(e) => log.error(s"Can't get a denormalized rule for a next order", e); None }
        .getOrElse(None)
        .getOrElse(DenormalizedMatchingRule.DefaultRule)

    result.copy(tickSize = result.tickSize max DenormalizedMatchingRule.DefaultTickSize)
  }

  def getNormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long): MatchingRule = {
    getDenormalizedRuleForNextOrder(assetPair, currentOffset).normalize(assetPair, blockchain)
  }

  def updateCurrentMatchingRule(assetPair: AssetPair, denormalizedMatchingRule: DenormalizedMatchingRule): Unit = {
    currentMatchingRule.put(assetPair, denormalizedMatchingRule)
  }

  def setCurrentMatchingRuleForNewOrderBook(assetPair: AssetPair, currentOffset: Long): Unit = {
    updateCurrentMatchingRule(assetPair, getDenormalizedRuleForNextOrder(assetPair, currentOffset))
  }
}
