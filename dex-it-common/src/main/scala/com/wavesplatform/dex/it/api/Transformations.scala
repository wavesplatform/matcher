package com.wavesplatform.dex.it.api

import cats.~>
import com.softwaremill.sttp.Response
import com.wavesplatform.dex.meta.getSimpleName

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

  val toSyncRaw: AsyncRaw ~> SyncRaw = λ[AsyncRaw ~> SyncRaw](sync(_))
  val toSyncHttp: AsyncRaw ~> SyncHttp = λ[AsyncRaw ~> SyncHttp](sync(_).response)
  val toSyncTry: AsyncRaw ~> SyncTry = λ[AsyncRaw ~> SyncTry](tryParse("tryGet", _)(_.tryGet))
  val toSyncUnsafe: AsyncRaw ~> SyncUnsafe = λ[AsyncRaw ~> SyncUnsafe](tryParse("unsafeGet", _)(_.unsafeGet))

  def sync[T](x: Future[T]): T =
    try Await.result(x, syncTimeout)
    catch {
      // Throw is required, otherwise we don't see a test code in the stacktrace
      case e: Throwable => throw new RuntimeException("An error during request", e)
    }

  def tryParse[EntityT, T](what: String, x: => Future[EnrichedResponse[ErrorT, EntityT]])(f: EnrichedResponse[ErrorT, EntityT] => T): T = {
    val syncX = sync(x)
    try f(syncX)
    catch {
      // Throw is required, otherwise we don't see a test code in the stacktrace
      case e: Throwable =>
        throw new RuntimeException(
          s"Can't get $what from HTTP response using ${getSimpleName(syncX.as)}.\nHTTP status: ${syncX.response.code} ${syncX.response.statusText}\nHTTP body: ${syncX.response.body}",
          e
        )
    }
  }

}
