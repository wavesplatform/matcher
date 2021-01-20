package com.wavesplatform.it.matcher.api.http

import com.wavesplatform.dex.it.api.{EnrichedResponse, RawHttpChecks}
import org.scalatest.freespec.AnyFreeSpec

trait ApiKeyHeaderChecks extends AnyFreeSpec with RawHttpChecks {

  val incorrectApiKeyHeader = Map("X-API-KEY" -> "incorrect")

  def shouldReturnErrorWithoutApiKeyHeader[ErrorT, EntityT](r: => EnrichedResponse[ErrorT, EntityT]) =
    "should return an error without X-API-KEY" in validateAuthorizationError(r)

  def shouldReturnErrorWithIncorrectApiKeyValue[ErrorT, EntityT](r: => EnrichedResponse[ErrorT, EntityT]) =
    "should return an error with incorrect X-API-KEY" in validateAuthorizationError(r)

}