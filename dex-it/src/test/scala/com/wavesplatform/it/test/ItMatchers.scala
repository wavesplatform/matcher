package com.wavesplatform.it.test

import com.wavesplatform.it.api.dex.MatcherError
import org.scalatest.matchers.Matcher

trait ItMatchers {
  def failWith(errorCode: Int): Matcher[Either[MatcherError, Any]]                      = new FailWith(errorCode)
  def failWith(errorCode: Int, messagePart: String): Matcher[Either[MatcherError, Any]] = new FailWith(errorCode, Some(messagePart))
  def failWith(errorCode: Int, containsParams: MatcherError.Params): Matcher[Either[MatcherError, Any]] =
    new FailWith(errorCode, None, containsParams)
}
