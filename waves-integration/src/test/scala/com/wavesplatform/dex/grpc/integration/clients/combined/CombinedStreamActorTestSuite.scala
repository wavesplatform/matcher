package com.wavesplatform.dex.grpc.integration.clients.combined

import akka.actor.ActorSystem
import akka.testkit.TestKitBase
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.{combined, ControlledStream}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import akka.actor.typed.scaladsl.adapter._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesControlledStream
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStreamActor.Command
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.matcherext.UtxEventsControlledStream
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.duration._
import java.util.concurrent.{Executors, ThreadLocalRandom}
import java.util.concurrent.atomic.AtomicInteger

final class CombinedStreamActorTestSuite extends WavesIntegrationSuiteBase with TestKitBase with BeforeAndAfterAll with Eventually {

  implicit override def system: ActorSystem = ActorSystem(getClass.getSimpleName)

  implicit private val ec = Scheduler(
    executor = Executors.newSingleThreadExecutor {
      new ThreadFactoryBuilder()
        .setDaemon(true)
        .setNameFormat("combined-stream-test-suite-%d")
        .build()
    },
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  "CombinedStreamActor" - {
    "don't loose updates from stash" in {
      val bu = mockBlockchainUpdatesControlledStream()
      val utx = mockUtxEventsControlledStream()
      val statusStream = ConcurrentSubject.publish[Status]
      val outputStream = ConcurrentSubject.publish[WavesNodeEvent]
      val ref = system.spawn(
        combined.CombinedStreamActor(CombinedStream.Settings(20.minutes), new AtomicInteger(5), bu, utx, statusStream, outputStream),
        s"combined-stream-${ThreadLocalRandom.current().nextInt()}"
      )
      ref ! Command.Start(1)
      ref ! Command.ProcessUtxEvent(UtxEvent(UtxEvent.Type.Switch(UtxEvent.Switch())))
      ref ! Command.ProcessUtxSystemEvent(SystemEvent.BecameReady)
      ref ! Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.BecameReady)

      val outputEvents = outputStream.takeByTimespan(patienceConfig.timeout).toListL.runSyncUnsafe()
      outputEvents.headOption shouldBe Some(WavesNodeEvent.UtxSwitched(Seq.empty))

    }
  }

  private def mockUtxEventsControlledStream() = new UtxEventsControlledStream() {
    override def start(): Unit = ()

    override val stream: Observable[UtxEvent] = ConcurrentSubject.publish[UtxEvent]
    override val systemStream: Observable[ControlledStream.SystemEvent] = ConcurrentSubject.publish[ControlledStream.SystemEvent]

    override def stop(): Unit = ()

    override def close(): Unit = ()
  }

  private def mockBlockchainUpdatesControlledStream() = new BlockchainUpdatesControlledStream() {
    override def startFrom(height: Int): Unit = ()

    override def requestNext(): Unit = ()

    override val stream: Observable[SubscribeEvent] = ConcurrentSubject.publish[SubscribeEvent]
    override val systemStream: Observable[ControlledStream.SystemEvent] = ConcurrentSubject.publish[ControlledStream.SystemEvent]

    override def stop(): Unit = ()

    override def close(): Unit = ()
  }

}
