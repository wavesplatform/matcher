package com.wavesplatform.dex.settings

import com.wavesplatform.dex.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.dex.settings.EventsQueueSettings.CircuitBreakerSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration

case class EventsQueueSettings(tpe: String,
                               local: LocalMatcherQueue.Settings,
                               kafka: KafkaMatcherQueue.Settings,
                               circuitBreaker: CircuitBreakerSettings)

object EventsQueueSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase

  implicit val eventsQueueSettingsReader: ValueReader[EventsQueueSettings] = { (cfg, path) =>
    EventsQueueSettings(
      tpe = cfg.getString(s"$path.type"),
      local = cfg.as[LocalMatcherQueue.Settings](s"$path.local"),
      kafka = cfg.as[KafkaMatcherQueue.Settings](s"$path.kafka"),
      circuitBreaker = cfg.as[CircuitBreakerSettings](s"$path.circuit-breaker")
    )
  }

  case class CircuitBreakerSettings(maxFailures: Int, callTimeout: FiniteDuration, resetTimeout: FiniteDuration)
}
