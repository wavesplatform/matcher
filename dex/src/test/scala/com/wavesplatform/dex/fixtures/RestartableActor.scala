package com.wavesplatform.dex.fixtures

import akka.actor.Actor
import com.wavesplatform.dex.fixtures.RestartableActor.{RestartActor, RestartActorException}

trait RestartableActor extends Actor {

  override def unhandled(message: Any): Unit = {
    message match {
      case RestartActor => throw RestartActorException
      case _ =>
    }
    super.unhandled(message)
  }

}

object RestartableActor {
  case object RestartActor

  private object RestartActorException extends Exception("Planned restart")
}
