package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.BehaviorsImplicits._
import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.testkit.TestProbe
import akka.actor.typed.scaladsl.adapter._
import com.wavesplatform.dex.time.SystemTime
import kamon.Kamon
import kamon.trace.Span
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StashContextPropagationSpec extends AnyFreeSpec with Matchers with SystemTime with MatcherSpecLike {

  import TestStashPropagationActor.Command

  "StashContextPropagationSpec" - {

    "context propagation for untyped actors" in test { probe =>
      system.actorOf(TestStashPropagationActor.untyped(probe.ref))
    }

    "context propagation for typed actors" in test { probe =>
      system.spawn(TestStashPropagationActor.typed(probe.ref), "TestStashPropagationActor").toClassic
    }
  }

  private def test(mkTestStashPropagationActor: TestProbe => ActorRef): Unit = {
    val n = 10
    val probe = TestProbe()
    val testStashPropagationActor = mkTestStashPropagationActor(probe)
    val messages = (0 until n).toList.map { i =>
      val span = Kamon.spanBuilder(i.toString).doNotTrackMetrics().ignoreParentFromContext().start()
      ContextualMessage(i, span, None)
    }
    messages.foreach { x =>
      Kamon.runWithSpan(x.spanBefore, finishSpan = true) {
        testStashPropagationActor ! Command.StashMessage(x)
      }
    }
    testStashPropagationActor ! Command.UnstashAllAndForward
    val receivedMessages = probe.expectMsgAllClassOf(List.fill(n)(classOf[ContextualMessage]): _*)
    probe.expectNoMessage()
    receivedMessages.map(_.spanBefore.trace.id.string) should contain theSameElementsAs
    receivedMessages.flatMap(_.spanAfter.map(_.trace.id.string))
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Kamon.init()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    Kamon.stop()
  }

}

final case class ContextualMessage(msg: Int, spanBefore: Span, spanAfter: Option[Span])

object TestStashPropagationActor {

  sealed trait Command

  object Command {
    case object UnstashAllAndForward extends Command
    final case class StashMessage(msg: ContextualMessage) extends Command
  }

  def untyped(forwardRef: ActorRef): Props = Props {
    new Actor with Stash {

      def forward: Receive = {
        case Command.StashMessage(x) =>
          forwardRef ! x.copy(spanAfter = Some(Kamon.currentSpan()))
      }

      override def receive: Receive = {
        case Command.StashMessage(_) =>
          stash()
        case Command.UnstashAllAndForward =>
          unstashAll()
          context.become(forward)
      }

    }
  }

  def typed(forwardRef: ActorRef): Behavior[Command] = {
    def forward: Behavior[Command] = Behaviors.receiveMessage[Command] {
      case Command.StashMessage(msg) =>
        forwardRef ! msg.copy(spanAfter = Some(Kamon.currentSpan()))
        Behaviors.same
      case Command.UnstashAllAndForward =>
        Behaviors.same
    }

    Behaviors.stashWithCtxPropagation[Command](Integer.MAX_VALUE) { stash =>
      Behaviors.receiveMessage {
        case cmd @ Command.StashMessage(_) =>
          stash.stash(cmd)
          Behaviors.same

        case Command.UnstashAllAndForward =>
          stash.unstashAll(forward)
      }
    }
  }

}
