package com.wavesplatform.dex.api.http.headers

import akka.http.scaladsl.model.headers._

import scala.util.Try

object `X-User-Public-Key` extends ModeledCustomHeaderCompanion[`X-User-Public-Key`] {
  final val headerName = "X-User-Public-Key"
  final override val name: String = headerName

  override def parse(value: String): Try[`X-User-Public-Key`] = Try(new `X-User-Public-Key`(value))
}

final class `X-User-Public-Key`(val value: String) extends ModeledCustomHeader[`X-User-Public-Key`] {
  override def companion: `X-User-Public-Key`.type = `X-User-Public-Key`
  override def renderInRequests: Boolean = true
  override def renderInResponses: Boolean = false
}
