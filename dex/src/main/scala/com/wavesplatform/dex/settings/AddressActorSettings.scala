package com.wavesplatform.dex.settings

import scala.concurrent.duration._

case class AddressActorSettings(wsMessagesInterval: FiniteDuration, batchCancelTimeout: FiniteDuration, maxActiveOrders: Int)

object AddressActorSettings {
  val default: AddressActorSettings = AddressActorSettings(100.milliseconds, 20.seconds, 200)
}
