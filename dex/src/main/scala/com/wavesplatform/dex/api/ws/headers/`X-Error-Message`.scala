package com.wavesplatform.dex.api.ws.headers

import akka.http.scaladsl.model.headers._

import scala.util.Try

object `X-Error-Message` extends ModeledCustomHeaderCompanion[`X-Error-Message`] {
  override final val name: String                           = "X-Error-Message"
  override def parse(value: String): Try[`X-Error-Message`] = Try(new `X-Error-Message`(value))
}

final class `X-Error-Message`(val value: String) extends ModeledCustomHeader[`X-Error-Message`] {
  override def companion: `X-Error-Message`.type = `X-Error-Message`
  override def renderInRequests: Boolean         = false
  override def renderInResponses: Boolean        = true
}
