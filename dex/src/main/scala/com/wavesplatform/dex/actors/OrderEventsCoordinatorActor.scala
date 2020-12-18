package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import com.wavesplatform.dex.model.Events

object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
  }

  def apply(addressDirectoryRef: classic.ActorRef): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage[Message] {
      case Command.Process(event) =>
        addressDirectoryRef ! event
        Behaviors.same

      case Command.ProcessError(event) =>
        addressDirectoryRef ! event
        Behaviors.same
    }
  }

}
