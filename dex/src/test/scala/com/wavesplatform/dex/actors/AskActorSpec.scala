package com.wavesplatform.dex.actors

import akka.actor.ActorRef
import akka.testkit.TestProbe
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.time.SystemTime
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, TimeoutException}

class AskActorSpec extends AnyFreeSpec with Matchers with SystemTime with MatcherSpecLike with DiffMatcherWithImplicits {

  private val defaultTimeout = 5.seconds
  private val defaultResponse = "foo"

  "AskActor" - {
    "happy path" in test { (ref, future) =>
      ref ! defaultResponse
      val actual = Await.result(future, defaultTimeout)
      actual should matchTo(defaultResponse)
    }

    "timeout" in test { (_, future) =>
      Await.result(future.failed, defaultTimeout) shouldBe a[TimeoutException]
    }

    "unexpected response type" in test { (ref, future) =>
      ref ! 100500
      Await.result(future.failed, defaultTimeout) shouldBe a[IllegalArgumentException]
    }
  }

  private def test(f: (ActorRef, Future[String]) => Unit): Unit = {
    val (ref, future) = AskActor.mk[String](100.millis)
    val p = TestProbe()
    p.watch(ref)

    f(ref, future)

    p.expectTerminated(ref, defaultTimeout)
  }

  override protected def actorSystemName: String = "AskActorSpec"
}
