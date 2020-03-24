package com.wavesplatform.dex.api.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object `X-Api-Key` extends ModeledCustomHeaderCompanion[`X-Api-Key`] {
  final val headerName            = "X-API-Key" // Constant that compatible with annotations
  override final val name: String = headerName

  override def parse(value: String): Try[`X-Api-Key`] = Try(new `X-Api-Key`(value))
}

final class `X-Api-Key`(val value: String) extends ModeledCustomHeader[`X-Api-Key`] {
  override def companion: `X-Api-Key`.type = `X-Api-Key`
  override def renderInRequests: Boolean   = true
  override def renderInResponses: Boolean  = false
}

// noinspection ScalaStyle
object api_key extends ModeledCustomHeaderCompanion[api_key] {
  override val name: String                       = "api_key"
  override def parse(value: String): Try[api_key] = Try(new api_key(value))
}

// noinspection ScalaStyle
final class api_key(val value: String) extends ModeledCustomHeader[api_key] {
  override def companion: api_key.type    = api_key
  override def renderInRequests: Boolean  = true
  override def renderInResponses: Boolean = false
}
