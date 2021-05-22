package com.wavesplatform.dex.grpc.integration.clients.combined

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.adapter._
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesControlledStream
import com.wavesplatform.dex.grpc.integration.clients.combined
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.{Settings, Status}
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.matcherext.UtxEventsControlledStream
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger

class AkkaCombinedStream(settings: Settings, blockchainUpdates: BlockchainUpdatesControlledStream, utxEvents: UtxEventsControlledStream)(implicit
  system: ActorSystem,
  monixScheduler: Scheduler
) extends CombinedStream {

  private val processedHeight = new AtomicInteger(0)

  private val statusStream = ConcurrentSubject.publish[Status]
  @volatile private var lastStatus: Status = Status.Starting()
  statusStream.doOnNext(x => Task { lastStatus = x }).lastL.runToFuture

  private val outputStream = ConcurrentSubject.publish[WavesNodeEvent]

  private val ref = system.spawn(
    combined.CombinedStreamActor(settings, processedHeight, blockchainUpdates, utxEvents, statusStream, outputStream),
    s"combined-stream-${ThreadLocalRandom.current().nextInt()}" // for unit tests
  )

  override def startFrom(height: Int): Unit = ref ! CombinedStreamActor.Command.Start(height)

  override def restart(): Unit = ref ! CombinedStreamActor.Command.Restart

  override def currentStatus: CombinedStream.Status = lastStatus

  override def updateProcessedHeight(height: Int): Unit = ref ! CombinedStreamActor.Command.UpdateProcessedHeight(height)

  override def currentProcessedHeight: Int = processedHeight.get()

  override val stream: Observable[WavesNodeEvent] = outputStream
}
