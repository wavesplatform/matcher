package com.wavesplatform.dex.api.ws.actors

import scala.concurrent.duration.FiniteDuration

case class WsHealthCheckSettings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)
