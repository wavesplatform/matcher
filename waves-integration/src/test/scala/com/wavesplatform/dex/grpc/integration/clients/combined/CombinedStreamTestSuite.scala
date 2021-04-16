package com.wavesplatform.dex.grpc.integration.clients.combined

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesControlledStream
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStreamTestSuite._
import com.wavesplatform.dex.grpc.integration.clients.matcherext.UtxEventsControlledStream
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedException
import org.scalatest.time.{Millis, Seconds, Span}

import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.chaining._

class CombinedStreamTestSuite extends WavesIntegrationSuiteBase with Eventually {

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(5, Millis))

  implicit private val runNow = Scheduler(
    executor = Executors.newSingleThreadExecutor {
      new ThreadFactoryBuilder()
        .setDaemon(true)
        .setNameFormat("combined-stream-test-suite-%d")
        .build()
    },
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  "CombinedStream" - {
    "this" - {
      "doesn't start streams" in {
        val t = mk()
        logged(t.blockchainUpdates.systemStream)(_.headOption shouldBe empty)
        logged(t.utxEvents.systemStream)(_.headOption shouldBe empty)
      }

      "the height hint is default" in {
        val t = mk()
        t.cs.currentProcessedHeight shouldBe 1
      }
    }

    "startFrom" - {
      "starts streams" in {
        val t = mk()
        t.cs.startFrom(10)
        logged(t.blockchainUpdates.systemStream)(_.head shouldBe SystemEvent.BecameReady)
        logged(t.utxEvents.systemStream)(_.head shouldBe SystemEvent.BecameReady)
      }

      "affects the recovery height hint" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.currentProcessedHeight shouldBe 9
      }
    }

    "updateHeightHint" - {
      "affects the recovery height" in {
        val t = mk()
        t.cs.updateProcessedHeight(10)
        t.cs.currentProcessedHeight shouldBe 10
      }
    }

    "restart" - {
      "stop blockchainUpdates" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.restart()
        logged(t.blockchainUpdates.systemStream)(_.contains(SystemEvent.Stopped))
      }

      "stops utxEvents" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.restart()
        logged(t.utxEvents.systemStream)(_.contains(SystemEvent.Stopped))
      }

      "doesn't affect the recovery height" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.currentProcessedHeight shouldBe 9

        t.cs.restart()
        t.cs.currentProcessedHeight shouldBe 9
      }
    }

    "events" - {
      "blockchainUpdates" - {
        "BecameReady - don't trigger utxEvents" in {
          val t = mk()
          t.blockchainUpdates.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.utxEvents.systemStream)(_.lastOption shouldBe empty)
        }

        "Stopped" - {
          "stops utxEvents" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)
            logged(t.utxEvents.systemStream)(_.contains(SystemEvent.Stopped))
          }

          "both recovered" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)

            logged(t.utxEvents.systemStream)(_.last shouldBe SystemEvent.BecameReady)
            eventually { // It takes some time
              logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.BecameReady)
            }
          }
        }

        "Closed" - {
          "closes utxEvents" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.close()
            logged(t.utxEvents.systemStream)(_.contains(SystemEvent.Closed))
          }

          "no recovery" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.close()
            Await.result(t.cs.lastStatus, 5.seconds) should matchTo[Status](Status.Closing(blockchainUpdates = true, utxEvents = true))
          }
        }
      }

      "utxEvents" - {
        "BecameReady - triggers start of blockchainUpdates" in {
          val t = mk()
          t.utxEvents.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.blockchainUpdates.systemStream)(_.head shouldBe SystemEvent.BecameReady)
        }

        "Stopped" - {
          "stops blockchainUpdates" in {
            val t = mkEventuallyWorking()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)
            logged(t.blockchainUpdates.systemStream)(_.contains(SystemEvent.Stopped))
          }

          "recovery started" in {
            val t = mkEventuallyWorking()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)
            logged(t.utxEvents.systemStream)(_.last shouldBe SystemEvent.BecameReady)
            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.BecameReady)
          }
        }

        "Closed" - {
          "closes blockchainUpdates" in {
            val t = mkEventuallyWorking()
            t.utxEvents.close()
            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.Closed)
          }

          "no recovery" in {
            val t = mkEventuallyWorking()
            t.utxEvents.close()
            Await.result(t.cs.lastStatus, 5.seconds) should matchTo[Status](Status.Closing(blockchainUpdates = true, utxEvents = true))
          }
        }
      }
    }
  }

  private def mk(): TestClasses = {
    val blockchainUpdates = new BlockchainUpdatesControlledStreamMock
    val utxEvents = new UtxEventsControlledStreamMock
    val cs = new CombinedStream(
      CombinedStream.Settings(restartDelay = 10.millis),
      blockchainUpdates = blockchainUpdates,
      utxEvents = utxEvents
    )
    new TestClasses(cs, blockchainUpdates, utxEvents)
  }

  private def mkEventuallyWorking(): TestClasses = mk().tap { x =>
    x.utxEvents.systemStream.onNext(SystemEvent.BecameReady)
    eventually {
      x.cs.blockchainStatus shouldBe Status.Working
    }
  }

  private def logged[T](subject: Observable[T])(f: List[T] => Unit): Unit = eventually {
    val xs = subject.takeByTimespan(200.millis).toListL.runSyncUnsafe()
    withClue(s"$xs: ") {
      try f(xs)
      catch {
        case e: Throwable => throw new TestFailedException("", e, 3)
      }
    }
  }

}

object CombinedStreamTestSuite {

  class TestClasses(
    val cs: CombinedStream,
    val blockchainUpdates: BlockchainUpdatesControlledStreamMock,
    val utxEvents: UtxEventsControlledStreamMock
  )

  class BlockchainUpdatesControlledStreamMock(implicit scheduler: Scheduler) extends BlockchainUpdatesControlledStream {
    override val stream = ConcurrentSubject.replay[SubscribeEvent]
    override val systemStream = ConcurrentSubject.replay[SystemEvent]

    override def startFrom(height: Int): Unit = systemStream.onNext(SystemEvent.BecameReady)
    override def requestNext(): Unit = {}
    override def stop(): Unit = systemStream.onNext(SystemEvent.Stopped)

    override def close(): Unit = {
      systemStream.onNext(SystemEvent.Closed)
      systemStream.onComplete()
    }

  }

  class UtxEventsControlledStreamMock(implicit scheduler: Scheduler) extends UtxEventsControlledStream {
    override val stream = ConcurrentSubject.replay[UtxEvent]
    override val systemStream = ConcurrentSubject.replay[SystemEvent]

    override def start(): Unit = systemStream.onNext(SystemEvent.BecameReady)
    override def stop(): Unit = systemStream.onNext(SystemEvent.Stopped)

    override def close(): Unit = {
      systemStream.onNext(SystemEvent.Closed)
      systemStream.onComplete()
    }

  }

}
