package com.wavesplatform.dex.it.api

import cats.~>
import com.softwaremill.sttp.Response

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}

class Transformations[ErrorT](syncTimeout: FiniteDuration = 10.minutes) {

  type AsyncRaw[EntityT] = Future[EnrichedResponse[ErrorT, EntityT]]

  type AsyncHttp[_] = Future[Response[String]]
  type AsyncTry[EntityT] = Future[Either[ErrorT, EntityT]]
  type AsyncUnsafe[EntityT] = Future[EntityT]

  type SyncRaw[EntityT] = EnrichedResponse[ErrorT, EntityT]
  type SyncHttp[_] = Response[String]
  type SyncTry[EntityT] = Either[ErrorT, EntityT]
  type SyncUnsafe[EntityT] = EntityT

  val toAsyncHttp: AsyncRaw ~> AsyncHttp = λ[AsyncRaw ~> AsyncHttp](_.map(_.response))
  val toAsyncTry: AsyncRaw ~> AsyncTry = λ[AsyncRaw ~> AsyncTry](_.map(_.tryGet))
  val toAsyncUnsafe: AsyncRaw ~> AsyncUnsafe = λ[AsyncRaw ~> AsyncUnsafe](_.map(_.unsafeGet))

  val toSyncRaw: AsyncRaw ~> SyncRaw = λ[AsyncRaw ~> SyncRaw] { x =>
    try Await.result(x, syncTimeout)
    catch {
      case e: Throwable => throw new RuntimeException("An error during request", e) // Otherwise we don't see a test code in the stacktrace
    }
  }

  val toSyncHttp: AsyncRaw ~> SyncHttp = λ[AsyncRaw ~> SyncHttp] { x =>
    try Await.result(x, syncTimeout).response
    catch {
      case e: Throwable => throw new RuntimeException("An error during request", e) // Otherwise we don't see a test code in the stacktrace
    }
  }

  val toSyncTry: AsyncRaw ~> SyncTry = λ[AsyncRaw ~> SyncTry] { x =>
    try Await.result(x.map(_.tryGet), syncTimeout)
    catch {
      case e: Throwable => throw new RuntimeException("An error during request", e) // Otherwise we don't see a test code in the stacktrace
    }
  }

  val toSyncUnsafe: AsyncRaw ~> SyncUnsafe = λ[AsyncRaw ~> SyncUnsafe] { x =>
    try Await.result(x.map(_.unsafeGet), syncTimeout)
    catch {
      case e: Throwable => throw new RuntimeException("An error during request", e) // Otherwise we don't see a test code in the stacktrace
    }
  }

}
