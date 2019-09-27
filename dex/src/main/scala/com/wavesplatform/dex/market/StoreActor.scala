package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.market.StoreActor._
import com.wavesplatform.dex.queue.QueueEvent

import scala.util.Success

class StoreActor(store: StoreEvent) extends Actor {
  import context.dispatcher

  override def receive: Receive = {
    case Command.Save(requestId, x) =>
      val s = sender()
      store(x)
        .transform {
          case Success(None) => Success(Some(error.FeatureDisabled))
          case Success(_)    => Success(None)
          case _             => Success(Some(error.CanNotPersistEvent))
        }
        .onComplete {
          case Success(Some(error)) => s ! Event.StoreFailed(requestId, error)
          case _                    =>
        }
  }
}

object StoreActor {
  def props(store: StoreEvent) = Props(new StoreActor(store))

  sealed trait Command
  object Command {
    case class Save(requestId: Long, inner: QueueEvent) extends Command
  }

  sealed trait Event
  object Event {
    case class StoreFailed(requestId: Long, reason: MatcherError) extends Event
  }
}
