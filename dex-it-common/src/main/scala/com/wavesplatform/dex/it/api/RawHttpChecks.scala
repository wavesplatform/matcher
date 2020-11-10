package com.wavesplatform.dex.it.api

import com.softwaremill.sttp.StatusCodes
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import org.scalatest.matchers.should.Matchers

trait RawHttpChecks extends Matchers {

  protected def validate200Json[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT = {
    r.response.code should be(StatusCodes.Ok)
    r.response.headers should contain("Content-Type", "application/json")
    r.unsafeGet
  }

  protected def validate301Redirect[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit =
    r.response.code should be(StatusCodes.MovedPermanently)

  protected def validateMatcherError[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], code: Int, error: Int, message: String): Unit = {
    r.response.code should be(code)
    r.response.headers should contain("Content-Type", "application/json")
    r.response.body should be leftSideValue

    r.tryGet match {
      case Left(MatcherError(e, m, _, _)) => e should be(error); m should be(message);
      case _ => assert(false, s"Unexpected response $r")
    }
  }

  protected def validate404Exception[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit = {
    r.response.code should be(StatusCodes.NotFound)
    r.response.headers should contain("Content-Type", "text/plain; charset=UTF-8")
    r.response.body should be(Left("The requested resource could not be found but may be available again in the future."))
  }

}
