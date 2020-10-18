package com.wavesplatform.dex.it

import cats.~>
import com.softwaremill.sttp.Response
import com.wavesplatform.dex.it.api.responses.dex.MatcherError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

package object api {
  type AsyncEnriched[T] = Future[EnrichedResponse[T]]

  type AsyncHttpResponse[_] = Future[Response[String]]
  type AsyncTry[T] = Future[Either[MatcherError, T]]
  type AsyncUnsafe[T] = Future[T]

  type SyncTry[T] = Either[MatcherError, T]
  type SyncUnsafe[T] = T
  type SyncRaw[_] = Response[String]

  implicit val toAsyncHttpResponse: AsyncEnriched ~> AsyncHttpResponse = λ[AsyncEnriched ~> AsyncHttpResponse](_.map(_.response))
  implicit val toAsyncTry: AsyncEnriched ~> AsyncTry = λ[AsyncEnriched ~> AsyncTry](_.map(_.tryGet))
  implicit val toAsyncUnsafe: AsyncEnriched ~> AsyncUnsafe = λ[AsyncEnriched ~> AsyncUnsafe](_.map(parseUnsafe))

  val syncTimeout = 10.minutes

  implicit val toSyncTry: AsyncEnriched ~> SyncTry = λ[AsyncEnriched ~> SyncTry] { x =>
    Await.result(x.map(_.tryGet), syncTimeout)
  }

  implicit val toSyncUnsafe: AsyncEnriched ~> SyncUnsafe = λ[AsyncEnriched ~> SyncUnsafe] { x =>
    Await.result(x.map(parseUnsafe), syncTimeout)
  }

  implicit val toSyncRaw: AsyncEnriched ~> SyncRaw = λ[AsyncEnriched ~> SyncRaw] { x =>
    Await.result(x, syncTimeout).response
  }

  def parseUnsafe[T](enriched: EnrichedResponse[T]): T = enriched.tryGet match {
    case Left(e) => throw new RuntimeException(s"An unexpected MatcherError: $e")
    case Right(x) => x
  }

}
