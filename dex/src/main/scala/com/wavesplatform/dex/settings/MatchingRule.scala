package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.model.MatcherModel
import com.wavesplatform.dex.model.MatcherModel.{Denormalization, Normalization}
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.AssetPair
import future.com.wavesplatform.settings.nonEmptyListReader
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

/** Normalized representation of the matching rule */
case class MatchingRule(startOffset: QueueEventWithMeta.Offset, normalizedTickSize: Long) {

  /**
    * Denormalizes matching rule
    * @param defaultTickSize lambda that describes returned tick size value when assets in the pair are absent from the blockchain
    */
  def denormalize(assetPair: AssetPair,
                  blockchain: Blockchain,
                  defaultTickSize: MatcherError => Double = _ => RawMatchingRule.DefaultTickSize): RawMatchingRule = {
    val denormalizedTickSize = Denormalization.denormalizePrice(normalizedTickSize, assetPair, blockchain)
    RawMatchingRule(
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
case class RawMatchingRule(startOffset: Long, tickSize: Double) {

  /** Normalizes raw (denormalized) matching rule */
  def normalize(assetPair: AssetPair, blockchain: Blockchain): MatchingRule = {
    val normalizedTickSize = Normalization.normalizePrice(tickSize, assetPair, MatcherModel.getPairDecimals(assetPair, blockchain).getOrElse(8 -> 8))
    MatchingRule(startOffset, normalizedTickSize)
  }
}

object RawMatchingRule {

  val DefaultTickSize: Double      = 0.00000001
  val DefaultRule: RawMatchingRule = RawMatchingRule(0L, DefaultTickSize)

  @annotation.tailrec
  def skipOutdated(currOffset: QueueEventWithMeta.Offset, rules: NonEmptyList[RawMatchingRule]): NonEmptyList[RawMatchingRule] =
    if (currOffset > rules.head.startOffset)
      rules.tail match {
        case x :: xs =>
          if (currOffset == x.startOffset) NonEmptyList(x, xs)
          else if (currOffset > x.startOffset) skipOutdated(currOffset, NonEmptyList(x, xs))
          else rules
        case _ => rules
      } else rules

  private implicit val rawMatchingRuleReader: ValueReader[RawMatchingRule] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    val offsetValidated   = cfgValidator.validateByPredicate[Long](s"$path.start-offset")(_ >= 0, "required 0 <= start offset")
    val tickSizeValidated = cfgValidator.validateByPredicate[Double](s"$path.tick-size")(_ > 0, "required 0 < tick size")

    (offsetValidated, tickSizeValidated) mapN RawMatchingRule.apply getValueOrThrowErrors
  }

  implicit val rawMatchingRuleNelReader: ValueReader[NonEmptyList[RawMatchingRule]] =
    nonEmptyListReader[RawMatchingRule].map { xs =>
      val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
      if (isStrictOrder) xs
      else throw new IllegalArgumentException(s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}")
    }
}
