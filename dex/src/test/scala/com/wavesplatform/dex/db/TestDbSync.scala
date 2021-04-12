package com.wavesplatform.dex.db

import cats.MonadError
import cats.syntax.functor._
import cats.syntax.monadError._

import java.util.concurrent.Semaphore

object TestDbSync {

  //we have to make it "global" because all Db's instance will share the same single thread levelDbEc
  //we can't use reentrant lock here because the "lock" can be released by a thread other than the owner
  private val fairLock = new Semaphore(1, true)

  //"dirty" synchronization in order to simulate levelDbEc behavior
  //we can't use cats.effect.concurrent.Semaphore due to its effectful creation and absence of fairness
  private[db] def sync[F[_]: MonadError[*[_], Throwable], A](a: F[A]): F[A] = {
    fairLock.acquire()
    a.map { av => fairLock.release(); av }.adaptError { case th => fairLock.release(); th }
  }

}
