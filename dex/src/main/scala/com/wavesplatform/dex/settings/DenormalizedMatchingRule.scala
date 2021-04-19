package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

/** Denormalized representation of the matching rule passed from the application.conf */
case class DenormalizedMatchingRule(startOffset: Long, tickSize: BigDecimal) {

  def normalize(amountAssetDecimals: Int, priceAssetDecimals: Int): MatchingRule =
    MatchingRule(
      startOffset,
      Normalization.normalizePrice(tickSize, amountAssetDecimals, priceAssetDecimals)
    )

}

object DenormalizedMatchingRule extends ScorexLogging {

  val DefaultTickSize: BigDecimal = 0.00000001

  implicit val deviationsConfigReader = semiauto
    .deriveReader[DenormalizedMatchingRule]
    .validatedField(
      validationOf.field[DenormalizedMatchingRule, "startOffset"].mk(x => rules.gtEq0(x.startOffset)),
      validationOf.field[DenormalizedMatchingRule, "tickSize"].mk(x => rules.gt0(x.tickSize))
    )

  def getDefaultRule(amountAssetDecimals: Int, priceAssetDecimals: Int): DenormalizedMatchingRule = {
    val DenormalizedMatchingRule(startOffset, tickSize) = MatchingRule.DefaultRule.denormalize(amountAssetDecimals, priceAssetDecimals)
    DenormalizedMatchingRule(startOffset, tickSize max DefaultTickSize)
  }

  @annotation.tailrec
  def skipOutdated(
    currOffset: ValidatedCommandWithMeta.Offset,
    rules: NonEmptyList[DenormalizedMatchingRule]
  ): NonEmptyList[DenormalizedMatchingRule] =
    if (currOffset > rules.head.startOffset)
      rules.tail match {
        case x :: xs =>
          if (currOffset == x.startOffset) NonEmptyList(x, xs)
          else if (currOffset > x.startOffset) skipOutdated(currOffset, NonEmptyList(x, xs))
          else rules
        case _ => rules
      }
    else rules

  /**
   * Returns denormalized (from application.conf) matching rules for the specified asset pair.
   * Prepends default rule if matching rules list doesn't contain element with startOffset = 0
   */
  def getDenormalizedMatchingRules(
    settings: MatcherSettings,
    assetPair: AssetPair,
    amountAssetDecimals: Int,
    priceAssetDecimals: Int
  ): NonEmptyList[DenormalizedMatchingRule] = {
    lazy val defaultRule = getDefaultRule(amountAssetDecimals, priceAssetDecimals)
    val rules = settings.matchingRules.getOrElse(assetPair, NonEmptyList.one(defaultRule))
    if (rules.head.startOffset == 0) rules else defaultRule :: rules
  }

}
