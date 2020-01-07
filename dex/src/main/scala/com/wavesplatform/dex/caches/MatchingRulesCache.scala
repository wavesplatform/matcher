package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import cats.data.NonEmptyList
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging

import scala.util.Try
import scala.util.control.NonFatal

class MatchingRulesCache(matcherSettings: MatcherSettings) extends ScorexLogging {

  private val allMatchingRules    = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair, assetDecimals: Asset => Int): NonEmptyList[DenormalizedMatchingRule] = {
    allMatchingRules.computeIfAbsent(
      assetPair,
      _ => DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, assetDecimals, assetPair)
    )
  }

  // DEX-488 TODO remove after found a reason of NPE
  def getDenormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long, assetDecimals: Asset => Int): DenormalizedMatchingRule = {

    lazy val defaultRule = DenormalizedMatchingRule.getDefaultRule(assetPair, assetDecimals)

    val result =
      Try {
        getMatchingRules(assetPair, assetDecimals).toList.reverse.collectFirst {
          case mr @ DenormalizedMatchingRule(startOffset, _) if startOffset <= (currentOffset + 1) => mr
        }
      }.recover { case NonFatal(e) => log.error(s"Can't get a denormalized rule for a next order", e); None }
        .getOrElse(None)
        .getOrElse(defaultRule)

    result.copy(tickSize = result.tickSize max defaultRule.tickSize)
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
