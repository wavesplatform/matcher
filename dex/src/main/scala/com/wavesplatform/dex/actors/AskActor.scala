package com.wavesplatform.dex.actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise, TimeoutException}
import scala.reflect.ClassTag

class AskActor[T](p: Promise[T], timeout: FiniteDuration)(implicit ct: ClassTag[T]) extends Actor {
  import context.dispatcher
  private val timeoutCancelable = context.system.scheduler.scheduleOnce(timeout, self, AskActor.timeoutMessage)

  override val receive: Receive = {
    case x => // Fix in Scala 2.13
      timeoutCancelable.cancel()
      context.stop(self)
      x match {
        case x: T if x.getClass == ct.runtimeClass => p.trySuccess(x)
        case e: Status.Failure                     => p.tryFailure(e.cause)
        case _                                     => p.tryFailure(new IllegalArgumentException(s"Expected ${ct.runtimeClass.getName}, but got $x"))
      }
  }
}

object AskActor {
  private val timeoutMessage = {
    val reason = new TimeoutException("Typed ask is timed out!")
    reason.setStackTrace(Array.empty)
    Status.Failure(reason)
  }

  def props[T](p: Promise[T], timeout: FiniteDuration)(implicit ct: ClassTag[T]) = Props(new AskActor(p, timeout))
  def mk[T](timeout: FiniteDuration)(implicit ct: ClassTag[T], system: ActorSystem): (ActorRef, Future[T]) = {
    val p   = Promise[T]()
    val ref = system.actorOf(props(p, timeout))
    (ref, p.future)
  }
}
