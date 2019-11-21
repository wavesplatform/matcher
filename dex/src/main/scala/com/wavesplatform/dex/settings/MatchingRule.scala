package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.model.MatcherModel
import com.wavesplatform.dex.model.MatcherModel.{Denormalization, Normalization}
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import future.com.wavesplatform.settings.nonEmptyListReader
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.util.Try

/** Normalized representation of the matching rule */
case class MatchingRule(startOffset: QueueEventWithMeta.Offset, tickSize: Long) {

  /**
    * Denormalizes matching rule
    * @param defaultTickSize lambda that describes returned tick size value when assets in the pair are absent from the blockchain
    */
  def denormalize(assetPair: AssetPair,
                  blockchain: Blockchain,
                  defaultTickSize: MatcherError => Double = _ => DenormalizedMatchingRule.DefaultTickSize): DenormalizedMatchingRule = {
    val denormalizedTickSize = Denormalization.denormalizePrice(tickSize, assetPair, blockchain)
    DenormalizedMatchingRule(
      startOffset = startOffset,
      tickSize = denormalizedTickSize.leftMap { defaultTickSize }.merge
    )
  }
}

object MatchingRule {
  val DefaultTickSize: Long     = 1
  val DefaultRule: MatchingRule = MatchingRule(0L, DefaultTickSize)
}

/** Denormalized representation of the matching rule passed from the application.conf */
case class DenormalizedMatchingRule(startOffset: Long, tickSize: Double) {

  def normalize(assetPair: AssetPair, blockchain: Blockchain): MatchingRule = {
    val normalizedTickSize = Normalization.normalizePrice(tickSize, assetPair, MatcherModel.getPairDecimals(assetPair, blockchain).getOrElse(8 -> 8))
    MatchingRule(startOffset, normalizedTickSize)
  }
}

object DenormalizedMatchingRule extends ScorexLogging {

  val DefaultTickSize: Double               = 0.00000001
  val DefaultRule: DenormalizedMatchingRule = DenormalizedMatchingRule(0L, DefaultTickSize)

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
    val tickSizeValidated = cfgValidator.validateByPredicate[Double](s"$path.tick-size")(_ > 0, "required 0 < tick size")

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
  def getDenormalizedMatchingRules(settings: MatcherSettings, blockchain: Blockchain, assetPair: AssetPair)(
      implicit errorFormatterContext: ErrorFormatterContext): NonEmptyList[DenormalizedMatchingRule] = {
    lazy val defaultRule =
      MatchingRule.DefaultRule.denormalize(
        assetPair,
        blockchain,
        defaultTickSize = { e =>
          // DEX-488 TODO remove after found a reason of NPE
          val errorMsg =
            s"""Can't convert matching rule for $assetPair: ${Try(e.mkMessage(errorFormatterContext).text).getOrElse(e.code)}.
               | Usually this happens when the blockchain was rolled back.""".stripMargin
          log.error(errorMsg)
          DenormalizedMatchingRule.DefaultTickSize
        }
      )

    val rules = settings.matchingRules.getOrElse(assetPair, NonEmptyList.one(defaultRule))
    if (rules.head.startOffset == 0) rules else defaultRule :: rules
  }
}
