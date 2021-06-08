package com.wavesplatform.dex.tools

import akka.actor.ActorSystem
import akka.testkit.TestKitBase
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.settings.{loadConfig, MatcherSettings, WaitingOffsetQueueSettings}
import com.wavesplatform.dex.tool.WaitOffsetTool
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

class WaitOffsetToolSpec extends AnyFreeSpec with ScalaFutures with TestKitBase with Matchers {

  implicit override lazy val system: ActorSystem = ActorSystem(
    getClass.getSimpleName,
    loadConfig(ConfigFactory.empty())
  )

  import system.dispatcher

  private val checkInterval = 100.millis
  private val waitingSettings = WaitingOffsetQueueSettings(5.seconds, 1, checkInterval)

  private val settings = ConfigSource
    .fromConfig(ConfigFactory.load())
    .at("waves.dex")
    .loadOrThrow[MatcherSettings]
    .copy(waitingQueueSettings = waitingSettings)

  private var lastOffset = 200L
  private var currentOffset = 170L

  private def getCurrentOffset: Long = currentOffset

  private def getLastOffset(d: Deadline): Future[Long] = Future.successful(lastOffset)

  "WaitOffsetTool" in {

    markup("start future with waiting")
    val future = WaitOffsetTool.waitOffsetReached(getLastOffset, getCurrentOffset, lastOffset, Deadline.now + 2.minutes, settings)
    future.isCompleted shouldBe false

    markup("not finish future if currentOffset is increased, but difference is too big")
    currentOffset = 190L
    Thread.sleep(checkInterval.toMillis)
    future.isCompleted shouldBe false

    markup("not finish future if lastOffset is increased, so difference is bigger")
    lastOffset = 250L
    Thread.sleep(checkInterval.toMillis)
    future.isCompleted shouldBe false

    markup("not finish future if currentOffset is increased after lastOffset was increased, but difference still big")
    currentOffset = 220L
    Thread.sleep(checkInterval.toMillis)
    future.isCompleted shouldBe false

    markup("successfully finish future if difference is small enough")
    currentOffset = 246L
    Thread.sleep(checkInterval.toMillis)
    future.isCompleted shouldBe true
    future.value.get shouldBe Success(())

  }

}
