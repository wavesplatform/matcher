package com.wavesplatform.dex.grpc.integration.clients.combined

import java.util.concurrent.atomic.AtomicBoolean

import cats.syntax.either._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesControlledStream, BlockchainUpdatesConversions}
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.matcherext.{UtxEventConversions, UtxEventsControlledStream}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.duration.FiniteDuration

class CombinedStream(
  settings: CombinedStream.Settings,
  blockchainUpdates: BlockchainUpdatesControlledStream,
  utxEvents: UtxEventsControlledStream
)(implicit scheduler: Scheduler)
    extends ScorexLogging {

  // Note, it is not a processed height! A less value could be emitted to control
  @volatile private var heightHint = 1

  @volatile private var recoverOnlyBlockchainUpdates = false

  private val blockchainUpdatesStopped = new AtomicBoolean(true)
  private val utxEventsStopped = new AtomicBoolean(true)

  private val blockchainUpdatesClosed = new AtomicBoolean(false)
  private val utxEventsClosed = new AtomicBoolean(false)

  private val internalStream = ConcurrentSubject.publish[WavesNodeEvent]
  val stream: Observable[WavesNodeEvent] = internalStream

  Observable(
    utxEvents.systemStream.map(_.asLeft[ControlledStream.SystemEvent]),
    blockchainUpdates.systemStream.map(_.asRight[ControlledStream.SystemEvent])
  ).merge.foreach {
    case Left(evt) =>
      evt match {
        case ControlledStream.SystemEvent.BecameReady =>
          utxEventsStopped.set(false)

        case ControlledStream.SystemEvent.Stopped =>
          if (utxEventsStopped.compareAndSet(false, true))
            if (blockchainUpdatesStopped.get) recover()
            else blockchainUpdates.stop()

        case ControlledStream.SystemEvent.Closed =>
          if (utxEventsClosed.compareAndSet(false, true))
            if (blockchainUpdatesClosed.get) finish()
            else blockchainUpdates.close()
      }
    case Right(evt) =>
      evt match {
        case ControlledStream.SystemEvent.BecameReady =>
          if (blockchainUpdatesStopped.compareAndSet(true, false) && utxEventsStopped.get)
            utxEvents.start()

        case ControlledStream.SystemEvent.Stopped =>
          if (blockchainUpdatesStopped.compareAndSet(false, true))
            if (utxEventsStopped.get || recoverOnlyBlockchainUpdates) recover()
            else utxEvents.stop()

        case ControlledStream.SystemEvent.Closed =>
          if (blockchainUpdatesClosed.compareAndSet(false, true))
            if (utxEventsClosed.get) finish()
            else utxEvents.close()
      }
  }

  blockchainUpdates.stream.foreach { evt =>
    evt.update.flatMap(BlockchainUpdatesConversions.toEvent) match {
      case Some(x) => internalStream.onNext(x)
      case None =>
        log.warn(s"Can't convert $evt to a domain event, asking next")
        blockchainUpdates.requestNext()
    }
  }

  utxEvents.stream.foreach { evt =>
    UtxEventConversions.toEvent(evt) match {
      case Some(x) => internalStream.onNext(x)
      case None =>
        log.warn(s"Can't convert $evt to a domain event")
        blockchainUpdates.requestNext()
    }
  }

  // TODO DEX-1034
  def startFrom(height: Int): Unit = {
    updateHeightHint(height)
    blockchainUpdates.startFrom(height)
  }

  def updateHeightHint(height: Int): Unit =
    heightHint = height

  def currentHeightHint: Int = heightHint

  def restartFrom(height: Int): Unit = {
    updateHeightHint(height)
    /* No need to restart UTX, because:
      1. All streams operate normally
      2. We force a roll back during recover(), so UTX events will be stashed */
    recoverOnlyBlockchainUpdates = true

    // Self-healed above
    if (settings.restartDelay.length == 0) blockchainUpdates.stop()
    else scheduler.scheduleOnce(settings.restartDelay)(blockchainUpdates.stop())
  }

  private def recover(): Unit = {
    recoverOnlyBlockchainUpdates = false

    // Note, heightHint remains
    val rollbackHeight = math.max(1, heightHint - 1)
    val restartHeight = heightHint

    internalStream.onNext(WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.Height(rollbackHeight)))

    if (settings.restartDelay.length == 0) blockchainUpdates.startFrom(restartHeight)
    else scheduler.scheduleOnce(settings.restartDelay)(blockchainUpdates.startFrom(restartHeight))
  }

  private def finish(): Unit = internalStream.onComplete()

}

object CombinedStream {
  case class Settings(restartDelay: FiniteDuration)
}
