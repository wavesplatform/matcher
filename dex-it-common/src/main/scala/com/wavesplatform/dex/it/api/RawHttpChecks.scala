package com.wavesplatform.dex.it.api

import com.softwaremill.sttp.StatusCodes
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

trait RawHttpChecks extends Matchers {

  protected def validate200Json[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT = {
    r.response.code should be(StatusCodes.Ok)
    r.response.headers should contain("Content-Type", "application/json")
    r.unsafeGet
  }

  protected def validate301Redirect[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit =
    r.response.code should be(StatusCodes.MovedPermanently)

  protected def validateMatcherError[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], c: Int, e: Int, m: String): Unit = {
    r.response.code should be(c)
    r.response.headers should contain("Content-Type", "application/json")
    r.response.body should be leftSideValue

    val b = Json.parse(r.response.body.left.get)
    (b \ "message").as[String] should be(m)
    (b \ "error").as[Int] should be(e)
  }

  protected def validate404Exception[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit = {
    r.response.code should be(StatusCodes.NotFound)
    r.response.headers should contain("Content-Type", "text/plain; charset=UTF-8")
    r.response.body should be(Left("The requested resource could not be found but may be available again in the future."))
  }

}
