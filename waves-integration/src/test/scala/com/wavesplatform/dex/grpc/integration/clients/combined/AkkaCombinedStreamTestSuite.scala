package com.wavesplatform.dex.grpc.integration.clients.combined

import akka.actor.ActorSystem
import akka.testkit.TestKitBase
import cats.syntax.option._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesControlledStream
import com.wavesplatform.dex.grpc.integration.clients.combined.AkkaCombinedStreamTestSuite._
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.matcherext.UtxEventsControlledStream
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Millis, Span}

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.util.chaining._

class AkkaCombinedStreamTestSuite extends WavesIntegrationSuiteBase with TestKitBase with BeforeAndAfterAll with Eventually {

  implicit override lazy val system: ActorSystem = ActorSystem(getClass.getSimpleName)

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(200, Millis), interval = Span(5, Millis))

  implicit private val runNow = Scheduler(
    executor = Executors.newSingleThreadExecutor {
      new ThreadFactoryBuilder()
        .setDaemon(true)
        .setNameFormat("akka-combined-stream-test-suite-%d")
        .build()
    },
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  "AkkaCombinedStream" - {
    "this" - {
      "doesn't start streams" in {
        val t = mk()
        logged(t.blockchainUpdates.systemStream)(_.headOption shouldBe empty)
        logged(t.utxEvents.systemStream)(_.headOption shouldBe empty)
      }

      "the height hint is default" in {
        val t = mk()
        eventually {
          t.cs.currentProcessedHeight shouldBe 0 // Because we haven't yet processed a block
        }
      }
    }

    "startFrom" - {
      "starts streams" in {
        val t = mk()
        t.cs.startFrom(10)
        logged(t.blockchainUpdates.systemStream)(_.headOption shouldBe SystemEvent.BecameReady.some)
        logged(t.utxEvents.systemStream)(_.headOption shouldBe SystemEvent.BecameReady.some)
      }

      "affects the recovery height hint" in {
        val t = mk()
        t.cs.startFrom(10)
        eventually {
          t.cs.currentProcessedHeight shouldBe 9
        }
      }
    }

    "updateHeightHint" - {
      "affects the recovery height" in {
        val t = mkEventuallyWorking()
        t.cs.updateProcessedHeight(10)
        eventually {
          t.cs.currentProcessedHeight shouldBe 10
        }
      }
    }

    "currentStatus" - {
      "returns correct working status" in {
        val t = mk()
        t.cs.startFrom(10)
        eventually {
          t.cs.currentStatus shouldBe Status.Working(9)
        }
      }

      "returns correct working status after updating processed height" in {
        val t = mkEventuallyWorking()
        t.cs.updateProcessedHeight(10)
        eventually {
          t.cs.currentStatus shouldBe Status.Working(10)
        }
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

      // because we use restart only during issues and we don't know if a new block came during the restart
      "decreases the recovery height" in {
        val t = mk()
        t.cs.startFrom(10)
        eventually {
          t.cs.currentProcessedHeight shouldBe 9
        }

        t.cs.restart()
        eventually {
          t.cs.currentProcessedHeight shouldBe 8
        }
      }
    }

    "events" - {
      "blockchainUpdates" - {
        "BecameReady - don't trigger" in {
          val t = mk()
          t.blockchainUpdates.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.utxEvents.systemStream)(_.lastOption shouldBe empty)
          t.cs.currentStatus shouldBe Status.Starting()
        }

        "Stopped" - {
          "stops utxEvents" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)
            logged(t.utxEvents.systemStream)(_.contains(SystemEvent.Stopped))
          }

          "eventually recovered" in {
            val t = mkEventuallyWorking()
            t.blockchainUpdates.systemStream.onNext(SystemEvent.Stopped)

            Thread.sleep(100) // Because Working happens multiple times
            eventually {
              t.cs.currentProcessedHeight shouldBe -1
            }
            eventually {
              t.cs.currentStatus should matchTo[Status](Status.Working(-1))
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
            eventually {
              t.cs.currentStatus should matchTo[Status](Status.Closing(
                blockchainUpdates = true,
                utxEvents = true
              ))
            }
          }
        }
      }

      "utxEvents" - {
        "BecameReady - doesn't trigger" in {
          val t = mk()
          t.utxEvents.systemStream.onNext(SystemEvent.BecameReady)
          logged(t.blockchainUpdates.systemStream)(_.headOption shouldBe empty)
          t.cs.currentStatus shouldBe Status.Starting()
        }

        "Stopped" - {
          "stops blockchainUpdates" in {
            val t = mkEventuallyWorking()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)
            logged(t.blockchainUpdates.systemStream)(_.contains(SystemEvent.Stopped))
          }

          "eventually recovered" in {
            val t = mkEventuallyWorking()
            t.utxEvents.systemStream.onNext(SystemEvent.Stopped)

            logged(t.utxEvents.systemStream)(_ should matchTo(List[SystemEvent](
              SystemEvent.BecameReady,
              SystemEvent.Stopped,
              SystemEvent.BecameReady
            )))
            logged(t.blockchainUpdates.systemStream)(_ should matchTo(List[SystemEvent](
              SystemEvent.BecameReady,
              SystemEvent.Stopped,
              SystemEvent.BecameReady
            )))

            Thread.sleep(100) // Because Working happens multiple times
            eventually {
              t.cs.currentProcessedHeight shouldBe -1
            }
            eventually {
              t.cs.currentStatus should matchTo[Status](Status.Working(-1))
            }
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
            eventually {
              t.cs.currentStatus should matchTo[Status](Status.Closing(
                blockchainUpdates = true,
                utxEvents = true
              ))
            }
          }
        }
      }
    }
  }

  override protected def afterAll(): Unit = {
    shutdown(system)
    super.afterAll()
  }

  private def mk(): TestClasses = {
    val blockchainUpdates = new BlockchainUpdatesControlledStreamMock
    val utxEvents = new UtxEventsControlledStreamMock
    val cs = new AkkaCombinedStream(
      CombinedStream.Settings(restartDelay = 1.millis),
      blockchainUpdates = blockchainUpdates,
      utxEvents = utxEvents
    )
    new TestClasses(cs, blockchainUpdates, utxEvents)
  }

  private def mkEventuallyWorking(): TestClasses = mk().tap { x =>
    x.cs.startFrom(1)
    eventually {
      x.cs.currentStatus shouldBe Status.Working(0)
    }
  }

  private def logged[T](subject: Observable[T])(f: List[T] => Unit): Unit = eventually {
    val xs = subject.takeByTimespan(patienceConfig.timeout).toListL.runSyncUnsafe()
    withClue(s"$xs: ") {
      try f(xs)
      catch {
        case e: Throwable => throw Option(e.getCause).getOrElse(e)
      }
    }
  }

}

object AkkaCombinedStreamTestSuite {

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
