package com.wavesplatform.dex.tool

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.{MatcherSettings, WaitingOffsetToolSettings}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import pureconfig.ConfigSource

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class WaitOffsetToolSpec extends ScalaTestWithActorTestKit(ManualTime.config) with AnyWordSpecLike with ScalaFutures with Matchers {

  private val checkInterval = 100.millis
  private val waitingSettings = WaitingOffsetToolSettings(5.seconds, checkInterval)

  private val matcherSettings = ConfigSource
    .fromConfig(ConfigFactory.load())
    .at("waves.dex")
    .loadOrThrow[MatcherSettings]
    .copy(waitingOffsetToolSettings = waitingSettings)

  private def getLastOffset(lo: Offset): Future[Long] = Future.successful(lo)

  private val manualTime: ManualTime = ManualTime()

  "WaitOffsetTool" should {
    implicit val ex: ExecutionContextExecutor = system.executionContext
    val deadline = Deadline.now + 2.minutes

    "finish future with waiting if currentOffset equals lastOffset at start" in {
      val lastOffset = 200L
      val currentOffset = 200L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          matcherSettings,
          system.classicSystem.scheduler
        )

      future.futureValue shouldBe ()
    }

    "finish future with waiting only if difference between lo and co small enough" in {
      val lastOffset = 200L
      @volatile var currentOffset = 100L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          matcherSettings,
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

    "update lastOffset value so if co is big enough for previous lo, but not new, future will not be finished" in {
      @volatile var lastOffset = 200L
      @volatile var currentOffset = 100L

      val future = new TestWaitOffsetTool()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          matcherSettings,
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

    "finish future if commandsPerSecond was increased" in {
      @volatile var lastOffset = 200L
      @volatile var currentOffset = 180L

      val future = new WaitOffsetToolWithVariableCps()
        .waitOffsetReached(
          getLastOffset(lastOffset),
          currentOffset,
          lastOffset,
          deadline,
          matcherSettings,
          system.classicSystem.scheduler
        )
      future.isCompleted shouldBe false

      // commandsPerSecond = 2; minimal difference is 10
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      // commandsPerSecond = 3; minimal difference is 15
      manualTime.timePasses(checkInterval)
      future.isCompleted shouldBe false

      // commandsPerSecond = 4; minimal difference is 20
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
