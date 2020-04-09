package com.wavesplatform.dex.actors

import akka.actor.{Actor, Props, Status}

import scala.concurrent.Promise
import scala.reflect.ClassTag

// TODO timeout
class AskActor[T](p: Promise[T])(implicit ct: ClassTag[T]) extends Actor {
  override def receive: Receive = {
    case x =>
      context.stop(self)
      x match {
        case x: T if x.getClass == ct.runtimeClass => p.trySuccess(x)
        case e: Status.Failure                     => p.tryFailure(e.cause)
        case _                                     => p.tryFailure(new IllegalArgumentException(s"Expected ${ct.runtimeClass.getName}, but got $x"))
      }
  }
}

object AskActor {
  def props[T](p: Promise[T])(implicit ct: ClassTag[T]) = Props(new AskActor(p))
}
