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
      NonEmptyList.fromListUnsafe(self.expectMsgType[Process].events.toList).map {
        case x: T => x
        case ev => fail(s"Expected ${ct.runtimeClass.getName}, given: $ev")
      }

    def expectFirstOec[T <: Events.Event](implicit ct: ClassTag[T]): T =
      self.expectMsgType[Process].events.toList match {
        case (head: T) :: _ => head
        case events => fail(s"Expected ${ct.runtimeClass.getName}, given: $events")
      }

  }

}
