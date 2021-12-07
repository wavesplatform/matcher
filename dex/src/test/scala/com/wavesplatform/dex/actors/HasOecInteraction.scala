package com.wavesplatform.dex.actors

import akka.testkit.TestProbe
import cats.data.NonEmptyList
import com.wavesplatform.dex.actors.events.OrderEventsCoordinatorActor.Command.Process
import com.wavesplatform.dex.model.Events
import org.scalatest.Suite

import scala.reflect.ClassTag

trait HasOecInteraction {
  this: Suite =>

  implicit final class TestProbeOps(val self: TestProbe) {

    def expectOecProcess[T <: Events.Event](implicit ct: ClassTag[T]): NonEmptyList[T] =
      loop(self.expectMsgType[Process].events.toList)

    private def loop[T <: Events.Event](events: List[Events.Event])(implicit ct: ClassTag[T]): NonEmptyList[T] =
      events match {
        case (xs: T) :: Nil => NonEmptyList.one(xs)
        case (xs: T) :: tail => xs :: loop[T](tail)
        case event => fail(s"Expected ${ct.runtimeClass.getName}, given: $event")
      }

    def expectFirstOec[T <: Events.Event](implicit ct: ClassTag[T]): T =
      self.expectMsgType[Process].events.toList match {
        case (head: T) :: _ => head
        case event => fail(s"Expected ${ct.runtimeClass.getName}, given: $event")
      }

  }

}
