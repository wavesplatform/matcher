package com.wavesplatform.dex.actors

import java.util.concurrent.TimeoutException

import akka.actor.ActorSystem

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

object ActorSystemOps {

  implicit final class ImplicitOps(val self: ActorSystem) extends AnyVal {

    def timeout(after: FiniteDuration): Future[Nothing] = {
      val failure = Promise[Nothing]()
      self.scheduler.scheduleOnce(after)(failure.failure(new TimeoutException))(self.dispatcher)
      failure.future
    }

  }

}
