package com.wavesplatform.dex.market

import akka.actor.Actor
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}

class StoreActor extends Actor {}
object StoreActor {
  sealed trait Command
  object Command {
    case class Save(inner: QueueEvent) extends Command
  }

  sealed trait Event
  object Event {
    case class SaveFailed()                      extends Event
    case class SavePassed(r: QueueEventWithMeta) extends Event
  }
}
