package com.wavesplatform.it.test

import com.wavesplatform.it.api.MatcherError
import com.wavesplatform.it.api.MatcherError.Params
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right}

class FailWith(expectedErrorCode: Int, expectedMessage: Option[String] = None, expectedParams: Params = Params()) extends Matcher[Either[MatcherError, Any]] {
  override def apply(input: Either[MatcherError, Any]): MatchResult = result(
    matches = input match {
      case Right(_) => false
      case Left(e)  => e.error == expectedErrorCode && expectedMessage.forall(_ == e.message) && Params.contains(e.params, expectedParams)
    },
    input
  )

  private def result(matches: Boolean, r: Either[MatcherError, Any]): MatchResult =
    MatchResult(
      matches = false,
      "expecting Left(MatcherError(errorCode={0}, params={1})) but got {2}",
      "got expected error",
      IndexedSeq(expectedErrorCode, expectedParams, r)
    )
}
