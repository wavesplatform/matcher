package com.wavesplatform.dex.actors

import akka.actor.ActorPath
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.{actor => classic}

object BroadcastActor {
  private type SubscriptionId = ActorPath

  sealed trait Subscription[-T] extends Product with Serializable {
    def id: SubscriptionId
    private[BroadcastActor] def send: T => Unit
  }

  private[BroadcastActor] object Subscription {
    case class Classic[-T](subscriber: classic.ActorRef, send: T => Unit) extends Subscription[T] {
      override val id: SubscriptionId = subscriber.path
    }

    case class Typed[-T](subscriber: ActorRef[Nothing], send: T => Unit) extends Subscription[T] {
      override val id: SubscriptionId = subscriber.path
    }
  }

  def routed[I](ref: classic.ActorRef): Subscription[I] = adapted(ref)(identity)
  def adapted[I](ref: classic.ActorRef)(transform: I => Any): Subscription[I] = new Subscription.Classic[I](ref, x => ref ! transform(x))
  def adapted[I, O](ref: ActorRef[O])(transform: I => O): Subscription[I]     = new Subscription.Typed[I](ref, x => ref ! transform(x))

  def apply[T](subscriptions: Subscription[T]*): Behavior[T] = Behaviors.setup[T] { _ =>
    def state(subscriptions: Map[SubscriptionId, Subscription[T]]): Behavior[T] =
      Behaviors
        .receiveMessage[T] { x =>
          subscriptions.values.foreach(_.send(x))
          Behaviors.same
        }
        .receiveSignal {
          case (_, Terminated(ws)) => state(subscriptions - ws.path)
        }

    state(subscriptions.map(x => x.id -> x).toMap)
  }
}
