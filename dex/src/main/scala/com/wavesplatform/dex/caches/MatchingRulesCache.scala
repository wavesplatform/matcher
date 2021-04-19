package com.wavesplatform.dex.caches

import cats.data.NonEmptyList
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}

import java.util.concurrent.ConcurrentHashMap
import scala.util.Try
import scala.util.control.NonFatal

class MatchingRulesCache(matcherSettings: MatcherSettings) extends ScorexLogging {

  private val allMatchingRules = new ConcurrentHashMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]
  private val currentMatchingRule = new ConcurrentHashMap[AssetPair, DenormalizedMatchingRule]

  def getMatchingRules(assetPair: AssetPair, amountAssetDecimals: Int, priceAssetDecimals: Int): NonEmptyList[DenormalizedMatchingRule] =
    allMatchingRules.computeIfAbsent(
      assetPair,
      _ => DenormalizedMatchingRule.getDenormalizedMatchingRules(matcherSettings, assetPair, amountAssetDecimals, priceAssetDecimals)
    )

  // DEX-488 TODO remove after found a reason of NPE
  def getDenormalizedRuleForNextOrder(
    assetPair: AssetPair,
    currentOffset: Long,
    amountAssetDecimals: Int,
    priceAssetDecimals: Int
  ): DenormalizedMatchingRule = {

    lazy val defaultRule = DenormalizedMatchingRule.getDefaultRule(amountAssetDecimals, priceAssetDecimals)

    val result =
      Try {
        getMatchingRules(assetPair, amountAssetDecimals, priceAssetDecimals).foldLeft(defaultRule) { case (acc, mr) =>
          if (mr.startOffset <= (currentOffset + 1)) mr else acc
        }
      }.recover { case NonFatal(e) => log.error(s"Can't get a denormalized rule for a next order", e); defaultRule }
        .getOrElse(defaultRule)

    result.copy(tickSize = result.tickSize max defaultRule.tickSize)
  }

  def getNormalizedRuleForNextOrder(assetPair: AssetPair, currentOffset: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): MatchingRule =
    getDenormalizedRuleForNextOrder(assetPair, currentOffset, amountAssetDecimals, priceAssetDecimals)
      .normalize(amountAssetDecimals, priceAssetDecimals)

  def updateCurrentMatchingRule(assetPair: AssetPair, denormalizedMatchingRule: DenormalizedMatchingRule): Unit =
    currentMatchingRule.put(assetPair, denormalizedMatchingRule)

  def setCurrentMatchingRuleForNewOrderBook(assetPair: AssetPair, currentOffset: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): Unit =
    updateCurrentMatchingRule(assetPair, getDenormalizedRuleForNextOrder(assetPair, currentOffset, amountAssetDecimals, priceAssetDecimals))

}
