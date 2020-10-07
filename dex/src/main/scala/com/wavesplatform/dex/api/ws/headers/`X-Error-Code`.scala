package com.wavesplatform.dex.api.ws.headers

import akka.http.scaladsl.model.headers._

import scala.util.Try

object `X-Error-Code` extends ModeledCustomHeaderCompanion[`X-Error-Code`] {
  final override val name: String = "X-Error-Code"
  override def parse(value: String): Try[`X-Error-Code`] = Try(new `X-Error-Code`(value))
}

final class `X-Error-Code`(val value: String) extends ModeledCustomHeader[`X-Error-Code`] {
  override def companion: `X-Error-Code`.type = `X-Error-Code`
  override def renderInRequests: Boolean = false
  override def renderInResponses: Boolean = true
}
