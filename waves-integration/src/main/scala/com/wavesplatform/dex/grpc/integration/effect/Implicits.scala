package com.wavesplatform.dex.grpc.integration.effect

import io.netty.util.concurrent.{Future => NettyFuture}

import scala.concurrent.{CancellationException, Future, Promise}

object Implicits {

  implicit final class NettyFutureOps[T](val self: NettyFuture[T]) extends AnyVal {

    def asScala: Future[T] = {
      val r = Promise[T]()
      self.addListener { (future: NettyFuture[T]) =>
        if (future.isSuccess) r.success(future.get())
        else if (future.isCancelled) r.failure(new CancellationException)
        else r.failure(future.cause())
      }
      r.future
    }

  }

}
