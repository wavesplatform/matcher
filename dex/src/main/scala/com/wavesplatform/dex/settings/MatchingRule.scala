package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.syntax.apply._
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.dex.settings.Implicits.nonEmptyListReader
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

/** Normalized representation of the matching rule */
case class MatchingRule(startOffset: QueueEventWithMeta.Offset, tickSize: Long) {

  def denormalize(assetPair: AssetPair, getAssetDecimals: Asset => Int): DenormalizedMatchingRule = {
    DenormalizedMatchingRule(
      startOffset,
      Denormalization.denormalizePrice(tickSize, getAssetDecimals(assetPair.amountAsset), getAssetDecimals(assetPair.priceAsset))
    )
  }
}

object MatchingRule {
  val DefaultTickSize: Long     = 1
  val DefaultRule: MatchingRule = MatchingRule(0L, DefaultTickSize)
}

/** Denormalized representation of the matching rule passed from the application.conf */
case class DenormalizedMatchingRule(startOffset: Long, tickSize: BigDecimal) {

  def normalize(assetPair: AssetPair, assetDecimals: Asset => Int): MatchingRule = {
    MatchingRule(
      startOffset,
      Normalization.normalizePrice(tickSize, assetDecimals(assetPair.amountAsset), assetDecimals(assetPair.priceAsset))
    )
  }
}

object DenormalizedMatchingRule extends ScorexLogging {

  val DefaultTickSize: BigDecimal = 0.00000001

  def getDefaultRule(assetPair: AssetPair, assetDecimals: Asset => Int): DenormalizedMatchingRule = {
    val DenormalizedMatchingRule(startOffset, tickSize) = MatchingRule.DefaultRule.denormalize(assetPair, assetDecimals)
    DenormalizedMatchingRule(startOffset, tickSize max DefaultTickSize)
  }

  @annotation.tailrec
  def skipOutdated(currOffset: QueueEventWithMeta.Offset, rules: NonEmptyList[DenormalizedMatchingRule]): NonEmptyList[DenormalizedMatchingRule] =
    if (currOffset > rules.head.startOffset)
      rules.tail match {
        case x :: xs =>
          if (currOffset == x.startOffset) NonEmptyList(x, xs)
          else if (currOffset > x.startOffset) skipOutdated(currOffset, NonEmptyList(x, xs))
          else rules
        case _ => rules
      } else rules

  private implicit val denormalizedMatchingRuleReader: ValueReader[DenormalizedMatchingRule] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    val offsetValidated   = cfgValidator.validateByPredicate[Long](s"$path.start-offset")(_ >= 0, "required 0 <= start offset")
    val tickSizeValidated = cfgValidator.validateByPredicate[BigDecimal](s"$path.tick-size")(_ > 0, "required 0 < tick size")

    (offsetValidated, tickSizeValidated) mapN DenormalizedMatchingRule.apply getValueOrThrowErrors
  }

  implicit val denormalizedMatchingRuleNelReader: ValueReader[NonEmptyList[DenormalizedMatchingRule]] =
    nonEmptyListReader[DenormalizedMatchingRule].map { xs =>
      val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
      if (isStrictOrder) xs
      else throw new IllegalArgumentException(s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}")
    }

  /**
    * Returns denormalized (from application.conf) matching rules for the specified asset pair.
    * Prepends default rule if matching rules list doesn't contain element with startOffset = 0
    */
  def getDenormalizedMatchingRules(settings: MatcherSettings,
                                   assetDecimals: Asset => Int,
                                   assetPair: AssetPair): NonEmptyList[DenormalizedMatchingRule] = {
    lazy val defaultRule = getDefaultRule(assetPair, assetDecimals)
    val rules            = settings.matchingRules.getOrElse(assetPair, NonEmptyList.one(defaultRule))
    if (rules.head.startOffset == 0) rules else defaultRule :: rules
  }
}
