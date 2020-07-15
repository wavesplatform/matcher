package com.wavesplatform.dex.actors

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.dex.actors.WatchDistributedCompletionActorSpecification._
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => DrivenPropertyChecks}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

class WatchDistributedCompletionActorSpecification
    extends TestKit(ActorSystem("WatchDistributedCompletionActorSpecification"))
    with AnyWordSpecLike
    with DrivenPropertyChecks
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender {

  "WatchDistributedCompletionActorSpecification" should {
    "respond" when {
      "no workers" in watcher(Set.empty).expectMsg(Symbol("pong"))

      "all work is done" in {
        val workers = (-1 to Random.nextInt(10)).map(_ => system.actorOf(Props(new PongActor))).toSet
        watcher(workers).expectMsg(Symbol("pong"))
      }

      val dyingTestGen = for {
        workingNumber <- Gen.choose(0, 5)
        dyingNumber   <- Gen.choose(if (workingNumber == 0) 1 else 0, 5)
        dieDelays     <- Gen.listOfN(dyingNumber, Gen.choose(0.millis, 50.millis))
      } yield {
        val working = (1 to workingNumber).map(_ => system.actorOf(Props(new PongActor)))
        val dying = (1 to dyingNumber).zip(dieDelays).map {
          case (_, dieDelay) => system.actorOf(Props(new DyingActor(dieDelay)))
        }
        (working ++ dying).toSet
      }

      "even several of workers is dead" in forAll(dyingTestGen) { workers =>
        watcher(workers).expectMsg(Symbol("pong"))
      }

      "even all workers are dead" in {
        val workers = (-1 to Random.nextInt(10)).map(_ => system.actorOf(Props(new DyingActor(0.seconds)))).toSet
        watcher(workers).expectMsg(Symbol("pong"))
      }

      "when a worker respond and die" in {
        val workers = (-1 to Random.nextInt(10)).map(_ => system.actorOf(Props(new PongAndDieActor))).toSet
        watcher(workers).expectMsg(Symbol("pong"))
      }

      "requests are timed out" in {
        val workers = (-1 to Random.nextInt(10)).map(_ => system.actorOf(Props(new IgnoringActor))).toSet
        watcher(workers, 50.millis).expectMsg(Symbol("pong"))
      }
    }
  }

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }
}

object WatchDistributedCompletionActorSpecification {
  def watcher(workers: Set[ActorRef], timeout: FiniteDuration = 1.minute)(implicit system: ActorSystem): TestProbe = {
    val p = TestProbe()
    system.actorOf(Props(new WatchDistributedCompletionActor(workers, p.ref, Symbol("ping"), Symbol("pong"), timeout)))
    p
  }

  class IgnoringActor extends Actor {
    override def receive: Receive = Actor.ignoringBehavior
  }

  class PongActor extends Actor {
    override def receive: Receive = {
      case Symbol("ping") => sender() ! Symbol("pong")
    }
  }

  class PongAndDieActor extends Actor {
    override def receive: Receive = {
      case Symbol("ping") =>
        sender() ! Symbol("pong")
        context.stop(self)
    }
  }

  class DyingActor(delay: FiniteDuration) extends Actor {
    import context.dispatcher

    if (delay.length == 0) context.stop(self)
    else context.system.scheduler.scheduleOnce(delay, self, PoisonPill)

    override def receive: Receive = Actor.ignoringBehavior
  }
}
