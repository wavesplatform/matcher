package com.wavesplatform.dex.api.websockets.actors

import scala.concurrent.duration.FiniteDuration

case class WsHealthCheckSettings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)
