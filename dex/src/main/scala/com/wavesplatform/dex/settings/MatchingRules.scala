package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.dex.queue.QueueEventWithMeta
import future.com.wavesplatform.settings.nonEmptyListReader
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator
import future.com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class MatchingRules(startOffset: QueueEventWithMeta.Offset, normalizedTickSize: Long)

object MatchingRules {
  val Default                                 = MatchingRules(0L, 1)
  val DefaultNel: NonEmptyList[MatchingRules] = NonEmptyList.one(Default)
}

case class RawMatchingRules(startOffset: Long, tickSize: Double)

object RawMatchingRules {

  def skipOutdated(currOffset: QueueEventWithMeta.Offset, rules: NonEmptyList[RawMatchingRules]): NonEmptyList[RawMatchingRules] =
    if (currOffset > rules.head.startOffset)
      rules.tail match {
        case x :: xs =>
          if (currOffset == x.startOffset) NonEmptyList(x, xs)
          else if (currOffset > x.startOffset) skipOutdated(currOffset, NonEmptyList(x, xs))
          else rules
        case _ => rules
      } else rules

  private implicit val rawMatchingRulesReader: ValueReader[RawMatchingRules] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    val offsetValidated   = cfgValidator.validateByPredicate[Long](s"$path.start-offset")(_ >= 0, "required 0 <= start offset")
    val tickSizeValidated = cfgValidator.validateByPredicate[Double](s"$path.tick-size")(_ > 0, "required 0 < tick size")

    (offsetValidated, tickSizeValidated) mapN RawMatchingRules.apply getValueOrThrowErrors
  }

  implicit val rawMatchingRulesNelReader: ValueReader[NonEmptyList[RawMatchingRules]] =
    nonEmptyListReader[RawMatchingRules].map { xs =>
      val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
      if (isStrictOrder) xs
      else throw new IllegalArgumentException(s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}")
    }
}
