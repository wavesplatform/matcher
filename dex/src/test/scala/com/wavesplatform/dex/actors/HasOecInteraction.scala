package com.wavesplatform.dex.actors

import akka.testkit.TestProbe
import com.wavesplatform.dex.actors.events.OrderEventsCoordinatorActor.Command.Process
import com.wavesplatform.dex.model.Events
import org.scalatest.Suite

import scala.reflect.ClassTag

trait HasOecInteraction {
  this: Suite =>

  implicit final class TestProbeOps(val self: TestProbe) {

    def expectOecProcess[T <: Events.Event](implicit ct: ClassTag[T]): T =
      self.expectMsgType[Process].event match {
        case event: T => event
        case event => fail(s"Expected ${ct.runtimeClass.getName}, given: $event")
      }

  }

}
