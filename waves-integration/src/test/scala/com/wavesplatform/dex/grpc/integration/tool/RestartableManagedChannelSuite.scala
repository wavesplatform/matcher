package com.wavesplatform.dex.grpc.integration.tool

import com.wavesplatform.dex.WavesIntegrationSuiteBase
import io.grpc.ManagedChannel
import org.scalamock.scalatest.MockFactory

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class RestartableManagedChannelSuite extends WavesIntegrationSuiteBase with MockFactory {

  "RestartableManagedChannel should" - {

    "getChannel" in {
      val channel = mock[ManagedChannel]
      val maker = mockFunction[ManagedChannel]
      maker.expects().returning(channel).once()
      val restartableManagedChannel = new RestartableManagedChannel(maker)
      restartableManagedChannel.getChannel shouldBe channel
    }

    "shutdown" in testShutdown { (awaitTime, restartableManagedChannel) =>
      restartableManagedChannel.shutdown(awaitTime)
    }

    "not do any ops after shutting down" in testShutdown { (awaitTime, restartableManagedChannel) =>
      restartableManagedChannel.shutdown(awaitTime)
      intercept[RuntimeException](restartableManagedChannel.restart())
      intercept[RuntimeException](restartableManagedChannel.getChannel)
      intercept[RuntimeException](restartableManagedChannel.shutdown(awaitTime))
    }

    "restart without triggering getChannel" in {
      val channel = mock[ManagedChannel]
      val maker = mockFunction[ManagedChannel]
      maker.expects().returning(channel).once()
      val restartableManagedChannel = new RestartableManagedChannel(maker)
      restartableManagedChannel.restart()
      restartableManagedChannel.getChannel shouldBe channel
    }

    "restart" in {
      val channel1 = mock[ManagedChannel]
      val channel2 = mock[ManagedChannel]
      val maker = mockFunction[ManagedChannel]
      maker.expects().returning(channel1).once()
      maker.expects().returning(channel2).once()
      (channel1.shutdown _).expects().returning(channel1).once()
      val restartableManagedChannel = new RestartableManagedChannel(maker)
      restartableManagedChannel.getChannel //force channel creation
      restartableManagedChannel.restart()
    }

    "stop current channel" in {
      val channel = mock[ManagedChannel]
      (channel.shutdown _).expects().returning(channel).once()
      val maker = mockFunction[ManagedChannel]
      maker.expects().returning(channel).once()
      val restartableManagedChannel = new RestartableManagedChannel(maker)
      restartableManagedChannel.getChannel //force channel creation
      restartableManagedChannel.stop()
    }
  }

  private def testShutdown(f: (Duration, RestartableManagedChannel) => Unit): Unit = {
    val awaitTime = 10.seconds
    val channel = mock[ManagedChannel]
    (channel.shutdown _).expects().returning(channel).once()
    (channel.awaitTermination(_: Long, _: TimeUnit))
      .expects(awaitTime.toMillis, TimeUnit.MILLISECONDS)
      .returning(true)
      .once()
    val maker = mockFunction[ManagedChannel]
    maker.expects().returning(channel).once()
    val restartableManagedChannel = new RestartableManagedChannel(maker)
    restartableManagedChannel.getChannel //force channel creation
    f(awaitTime, restartableManagedChannel)
  }

}
