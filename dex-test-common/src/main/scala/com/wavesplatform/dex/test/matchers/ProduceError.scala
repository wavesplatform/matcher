package com.wavesplatform.dex.test.matchers

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.matching.Regex
import scala.util.{Left, Right}

object ProduceError {

  def produce(errorPattern: Regex): Matcher[Either[_, _]] = {
    case r @ Right(_) => MatchResult(matches = false, "expecting {0} to be Left and match: {1}", "got expected error", IndexedSeq(r, errorPattern))
    case Left(l) =>
      MatchResult(
        matches = errorPattern.findFirstIn(l.toString).isDefined,
        rawFailureMessage = "expecting {0} to match: {1}",
        rawNegatedFailureMessage = "got expected error",
        args = IndexedSeq(l, errorPattern)
      )
  }

  def produce(errorMessage: String): Matcher[Either[_, _]] = {
    case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
    case l @ Left(_) =>
      MatchResult(
        matches = l.toString contains errorMessage,
        rawFailureMessage = "expecting Left(...{0}...) but got {1}",
        rawNegatedFailureMessage = "got expected error",
        args = IndexedSeq(errorMessage, l)
      )
  }
}
