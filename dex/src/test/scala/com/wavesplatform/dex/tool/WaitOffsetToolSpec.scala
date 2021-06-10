package com.wavesplatform.dex.tool

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.{MatcherSettings, WaitingOffsetToolSettings}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class WaitOffsetToolSpec extends ScalaTestWithActorTestKit(ManualTime.config) with AnyFreeSpecLike with ScalaFutures with Matchers {

  private val checkInterval = 100.millis
  private val waitingSettings = WaitingOffsetToolSettings(5.seconds, checkInterval)

  private val matcherSettings = ConfigSource
    .fromConfig(ConfigFactory.load())
    .at("waves.dex")
    .loadOrThrow[MatcherSettings]
    .copy(waitingOffsetToolSettings = waitingSettings)

  @volatile private var lastOffset = 200L
  @volatile private var currentOffset = 170L

  private def getCurrentOffset: Long = currentOffset

  private def getLastOffset: Future[Long] = Future.successful(lastOffset)

  private val manualTime: ManualTime = ManualTime()

  "WaitOffsetTool" in {
    implicit val ex: ExecutionContextExecutor = system.executionContext

    markup("start future with waiting")
    val future = new TestWaitOffsetTool()
      .waitOffsetReached(
        getLastOffset,
        getCurrentOffset,
        lastOffset,
        Deadline.now + 2.minutes,
        matcherSettings,
        system.classicSystem.scheduler
      )

    future.isCompleted shouldBe false

    markup("not finish future if currentOffset is increased, but difference is too big")
    currentOffset = 190L
    manualTime.timePasses(checkInterval)
    future.isCompleted shouldBe false

    markup("not finish future if lastOffset is increased, so difference is bigger")
    lastOffset = 250L
    manualTime.timePasses(checkInterval)
    future.isCompleted shouldBe false

    markup("not finish future if currentOffset is increased after lastOffset was increased, but difference still big")
    currentOffset = 220L
    manualTime.timePasses(checkInterval)
    future.isCompleted shouldBe false

    markup("successfully finish future if difference is small enough")
    currentOffset = 246L
    manualTime.timePasses(checkInterval)
    future.futureValue shouldBe ()

  }

  private class TestWaitOffsetTool extends WaitOffsetTool {

    override def calcCommandsPerSecond(prevOffsetAndTime: OffsetAndTime, lastProcessedOffset: Offset): Double = 1

  }

}
