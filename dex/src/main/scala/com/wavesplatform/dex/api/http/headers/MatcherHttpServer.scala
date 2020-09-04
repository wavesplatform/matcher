package com.wavesplatform.dex.api.http.headers

import akka.http.scaladsl.model.headers._

import scala.util.Try

object MatcherHttpServer extends ModeledCustomHeaderCompanion[MatcherHttpServer] {
  final val headerName = "matcher-http-server" // Constant that compatible with annotations

  override final val name: String                                 = headerName
  override def parse(value: String): Try[MatcherHttpServer] = Try(new MatcherHttpServer(value))
}

final class MatcherHttpServer(val value: String) extends ModeledCustomHeader[MatcherHttpServer] {
  override def companion: MatcherHttpServer.type = MatcherHttpServer
  override def renderInRequests: Boolean               = true
  override def renderInResponses: Boolean              = false
}
