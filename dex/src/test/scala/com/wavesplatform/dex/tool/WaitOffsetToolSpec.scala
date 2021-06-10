package com.wavesplatform.dex.tool

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit}
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.WaitingOffsetToolSettings
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class WaitOffsetToolSpec extends ScalaTestWithActorTestKit(ManualTime.config) with AnyWordSpecLike with ScalaFutures with Matchers {

  private val checkInterval = 100.millis
  private val queueTimeout = 300.seconds
  private val deadline = Deadline.now + queueTimeout
  private val waitingSettings = WaitingOffsetToolSettings(queueTimeout, 5.seconds, checkInterval)

  private def getLastOffset(lo: Offset): Future[Long] = Future.successful(lo)

  private val manualTime: ManualTime = ManualTime()

  "WaitOffsetTool" should {
    implicit val ec: ExecutionContextExecutor = system.executionContext

    "finish awaiting offsets future if currentOffset equals lastOffset at the beginning" in {
      val lastOffset = 200L
      val currentOffset = 200L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          waitingSettings,
          system.classicSystem.scheduler
        )

      future.futureValue shouldBe ()
    }

    "finish awaiting offsets future if the difference between lo and co small enough" in {
      val lastOffset = 200L
      @volatile var currentOffset = 100L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          waitingSettings,
          system.classicSystem.scheduler
        )
      future.isCompleted shouldBe false

      currentOffset = 150L
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      currentOffset = 198L
      manualTime.timePasses(checkInterval)
      future.futureValue shouldBe ()

    }

    "use always fresh lo and co during awaiting offsets" in {
      @volatile var lastOffset = 200L
      @volatile var currentOffset = 100L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          waitingSettings,
          system.classicSystem.scheduler
        )
      future.isCompleted shouldBe false

      currentOffset = 198L
      lastOffset = 250L
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      currentOffset = 259L
      manualTime.timePasses(checkInterval)
      future.futureValue shouldBe ()

    }

    "finish awaiting offsets future when commandsPerSecond was increased several times" in {
      val lastOffset = 200L
      val currentOffset = 180L

      val future = new WaitOffsetToolWithVariableCps()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          waitingSettings,
          system.classicSystem.scheduler
        )
      future.isCompleted shouldBe false

      // commandsPerSecond = 2; lo - co / k = (20 / 2 = 10)  > 5 seconds
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      // commandsPerSecond = 3;  lo - co / k = (20 / 3 ~ 6)  > 5 seconds
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      // commandsPerSecond = 4; k = (20 / 4 = 5)  == 5 seconds
      manualTime.timePasses(checkInterval)
      future.futureValue shouldBe ()

    }
  }

  private class WaitOffsetToolWithVariableCps extends WaitOffsetTool {
    import WaitOffsetTool.OffsetAndTime

    @volatile private var cps = 1

    override def calcCommandsPerSecond(prevOffsetAndTime: OffsetAndTime, lastProcessedOffset: Offset): Double = {
      cps += 1
      cps
    }

  }

  private class TestWaitOffsetTool extends WaitOffsetTool {
    import WaitOffsetTool.OffsetAndTime

    override def calcCommandsPerSecond(prevOffsetAndTime: OffsetAndTime, lastProcessedOffset: Offset): Double = 1

  }

}
