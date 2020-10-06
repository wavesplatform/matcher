package com.wavesplatform.dex.settings

import com.wavesplatform.dex.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.dex.settings.EventsQueueSettings.CircuitBreakerSettings

import scala.concurrent.duration.FiniteDuration

case class EventsQueueSettings(
  `type`: String,
  local: LocalMatcherQueue.Settings,
  kafka: KafkaMatcherQueue.Settings,
  circuitBreaker: CircuitBreakerSettings
)

object EventsQueueSettings {
  case class CircuitBreakerSettings(maxFailures: Int, callTimeout: FiniteDuration, resetTimeout: FiniteDuration)
}
