package com.wavesplatform.dex.it.matchers

import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.api.responses.dex.MatcherError.Params
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right}

class FailWith(expectedErrorCode: Int, expectedMessagePart: Option[String] = None, expectedParams: Params = Params())
    extends Matcher[Either[MatcherError, Any]] {

  override def apply(input: Either[MatcherError, Any]): MatchResult = result(
    matches = input match {
      case Right(_) => false
      case Left(e) =>
        e.error == expectedErrorCode && expectedMessagePart.forall(e.message.contains) && (
          expectedParams.isEmpty || e.params.exists(Params.contains(_, expectedParams))
        )
    },
    input
  )

  private def result(matches: Boolean, r: Either[MatcherError, Any]): MatchResult =
    MatchResult(
      matches = matches,
      s"expecting Left(MatcherError(errorCode={0}, expectedMessagePart={1} params={2})) but got {3}",
      "got expected error",
      IndexedSeq(expectedErrorCode, expectedMessagePart, expectedParams, r)
    )

}
