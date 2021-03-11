package com.wavesplatform.dex.it.api

import sttp.model.{Header, MediaType, StatusCode}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import org.scalatest.matchers.should.Matchers

trait RawHttpChecks extends Matchers {

  val jsonHeader = Header.contentType(MediaType.ApplicationJson)

  private def validateResponseContainHeaders[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], expected: Seq[Header]): Unit =
    expected.foreach(r.response.headers should contain(_))

  private def validateResponse[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], code: StatusCode, headers: Seq[Header]): EntityT = {
    r.response.code should be(code)
    validateResponseContainHeaders(r, headers)
    r.unsafeGet
  }

  private def validateError[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], code: StatusCode): Unit = {
    r.response.code should be(code)
    validateResponseContainHeaders(r, Seq(jsonHeader))
    r.response.body should be leftSideValue
  }

  protected def validate200Hocon[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT =
    validateResponse(r, StatusCode.Ok, Seq(Header.contentType(MediaType("application", "hocon"))))

  protected def validate200Json[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT =
    validateResponse(r, StatusCode.Ok, Seq(jsonHeader))

  protected def validate201Json[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT =
    validateResponse(r, StatusCode.Created, Seq(jsonHeader))

  protected def validate202Json[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): EntityT =
    validateResponse(r, StatusCode.Accepted, Seq(jsonHeader))

  protected def validate301Redirect[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit =
    r.response.code should be(StatusCode.MovedPermanently)

  protected def validateIncorrectSignature[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]) =
    validateMatcherError(r, StatusCode.BadRequest, 1051904, "The request has an invalid signature")

  protected def validateAuthorizationError[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit =
    validateMatcherError(r, StatusCode.Forbidden, 106954752, "Provided API key is not correct")

  protected def validateMatcherErrorContainText[ErrorT, EntityT](
    r: EnrichedResponse[ErrorT, EntityT],
    code: StatusCode,
    error: Int,
    message: String
  ): Unit = {
    validateError(r, code)

    r.tryGet match {
      case Left(MatcherError(e, m, _, _)) => e should be(error); m.contains(message) should be(true);
      case _ => fail(s"Unexpected response $r")
    }
  }

  protected def validateMatcherError[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT], code: StatusCode, error: MatcherError): Unit = {
    validateError(r, code)

    r.tryGet match {
      case Left(MatcherError(e, m, s, _)) => e should be(error.error); m.contains(error.message) should be(true); s should be(error.status)
      case _ => fail(s"Unexpected response $r")
    }
  }

  protected def validateMatcherError[ErrorT, EntityT](
    r: EnrichedResponse[ErrorT, EntityT],
    code: StatusCode,
    error: Int,
    message: String
  ): Unit = {
    validateError(r, code)

    r.tryGet match {
      case Left(MatcherError(e, m, _, _)) => e should be(error); m should be(message);
      case _ => fail(s"Unexpected response $r")
    }
  }

  protected def validate404Exception[ErrorT, EntityT](r: EnrichedResponse[ErrorT, EntityT]): Unit = {
    r.response.code should be(StatusCode.NotFound)
    validateResponseContainHeaders(r, Seq(Header.contentType(MediaType.TextPlainUtf8)))
    r.response.body should be(Left("The requested resource could not be found but may be available again in the future."))
  }

}
