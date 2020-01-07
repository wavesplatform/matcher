package com.wavesplatform.dex.error

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.matching.Regex
import scala.util.{Left, Right}

class ProduceError(errorPattern: Regex) extends Matcher[Either[_, _]] {

  override def apply(ei: Either[_, _]): MatchResult =
    ei match {
      case r @ Right(_) => MatchResult(matches = false, "expecting {0} to be Left and match: {1}", "got expected error", IndexedSeq(r, errorPattern))
      case Left(l) =>
        MatchResult(matches = errorPattern.findFirstIn(l.toString).isDefined,
                    "expecting {0} to match: {1}",
                    "got expected error",
                    IndexedSeq(l, errorPattern))
    }
}

object ProduceError {
  def produce(errorPattern: Regex): Matcher[Either[_, _]] = new ProduceError(errorPattern)
  def produce(error: String): Matcher[Either[_, _]]       = new ProduceError(error.r)
}
