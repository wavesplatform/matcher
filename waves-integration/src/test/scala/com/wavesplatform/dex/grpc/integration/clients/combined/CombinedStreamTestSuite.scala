package com.wavesplatform.dex.grpc.integration.clients.combined

import cats.syntax.option._
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesControlledStream
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStreamTestSuite._
import com.wavesplatform.dex.grpc.integration.clients.matcherext.UtxEventsControlledStream
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.duration.DurationInt
import scala.util.chaining._

class CombinedStreamTestSuite extends WavesIntegrationSuiteBase {

  implicit private val runNow = Scheduler(
    ec = scala.concurrent.ExecutionContext.Implicits.global,
    executionModel = ExecutionModel.BatchedExecution(100)
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
        t.cs.currentHeightHint shouldBe 1
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
        t.cs.currentHeightHint shouldBe 10
      }
    }

    "updateHeightHint" - {
      "affects the recovery height" in {
        val t = mk()
        t.cs.updateHeightHint(10)
        t.cs.currentHeightHint shouldBe 10
      }
    }

    "restartFrom" - {
      "stop blockchainUpdates" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.restartFrom(5)
        logged(t.blockchainUpdates.systemStream)(_.tail.head shouldBe SystemEvent.Stopped)
      }

      "doesn't stop utxEvents" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.restartFrom(5)
        logged(t.utxEvents.systemStream)(_.tail shouldBe empty)
      }

      "affects the recovery height" in {
        val t = mk()
        t.cs.startFrom(10)
        t.cs.restartFrom(5)
        t.cs.currentHeightHint shouldBe 5
      }
    }

    "events" - {
      "blockchainUpdates" - {
        "BecameReady - triggers start of utxEvents" in {
          val t = mk()
          t.blockchainUpdates.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.utxEvents.systemStream)(_.head shouldBe SystemEvent.BecameReady)
        }

        "Stopped" - {
          "stops utxEvents" in {
            val t = mkStarted()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)
            logged(t.utxEvents.systemStream)(_.tail.head shouldBe SystemEvent.Stopped)
          }

          "both recovered" in {
            val t = mkStarted()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)

            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.BecameReady)
            logged(t.utxEvents.systemStream)(_.last shouldBe SystemEvent.BecameReady)
          }
        }

        "Closed" - {
          "closes utxEvents" in {
            val t = mkStarted()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Closed)
            logged(t.utxEvents.systemStream)(_.last shouldBe SystemEvent.Closed)
          }

          "no recovery" in {
            val t = mkStarted()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Closed)
            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.Closed)
          }
        }
      }

      "utxEvents" - {
        "BecameReady - don't trigger blockchainUpdates" in {
          val t = mk()
          t.utxEvents.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.blockchainUpdates.systemStream)(_.lastOption shouldBe empty)
        }

        "Stopped" - {
          "stops blockchainUpdates" in {
            val t = mkStarted()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)
            logged(t.blockchainUpdates.systemStream)(_.tail.headOption shouldBe SystemEvent.Stopped.some)
          }

          "recovery started" in {
            val t = mkStarted()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)
            t.utxEvents.systemStream.lastUnsafe shouldBe SystemEvent.BecameReady.some
            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.BecameReady)
          }
        }

        "Closed" - {
          "closes blockchainUpdates" in {
            val t = mkStarted()
            t.utxEvents.systemStream.onNext(SystemEvent.Closed)
            logged(t.blockchainUpdates.systemStream)(_.last shouldBe SystemEvent.Closed)
          }

          "no recovery" in {
            val t = mkStarted()
            t.utxEvents.systemStream.onNext(SystemEvent.Closed)
            logged(t.utxEvents.systemStream)(_.last shouldBe SystemEvent.Closed)
          }
        }
      }
    }
  }

  private def mk(): TestClasses = {
    val blockchainUpdates = new BlockchainUpdatesControlledStreamMock
    val utxEvents = new UtxEventsControlledStreamMock
    val cs = new CombinedStream(
      CombinedStream.Settings(restartDelay = 0.millis),
      blockchainUpdates = blockchainUpdates,
      utxEvents = utxEvents
    )
    new TestClasses(cs, blockchainUpdates, utxEvents)
  }

  private def mkStarted(): TestClasses = mk().tap { t =>
    t.utxEvents.systemStream.onNext(SystemEvent.BecameReady)
    t.blockchainUpdates.systemStream.onNext(SystemEvent.BecameReady)
  }

  private def logged[T](subject: Observable[T])(f: List[T] => Unit): Unit = {
    val xs = subject.toListLUnsafe
    withClue(s"$xs: ") {
      f(xs)
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
    override def close(): Unit = systemStream.onNext(SystemEvent.Closed)
  }

  class UtxEventsControlledStreamMock(implicit scheduler: Scheduler) extends UtxEventsControlledStream {
    override val stream = ConcurrentSubject.replay[UtxEvent]
    override val systemStream = ConcurrentSubject.replay[SystemEvent]

    override def start(): Unit = systemStream.onNext(SystemEvent.BecameReady)
    override def stop(): Unit = systemStream.onNext(SystemEvent.Stopped)
    override def close(): Unit = systemStream.onNext(SystemEvent.Closed)
  }

  implicit final class ObservableTestOps[T](val self: Observable[T]) extends AnyVal {
    def toListLUnsafe(implicit scheduler: Scheduler): List[T] = self.takeByTimespan(100.millis).toListL.runSyncUnsafe()
    def firstUnsafe(implicit scheduler: Scheduler): Option[T] = toListLUnsafe.headOption
    def lastUnsafe(implicit scheduler: Scheduler): Option[T] = toListLUnsafe.lastOption
  }

}
