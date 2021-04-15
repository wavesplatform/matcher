package com.wavesplatform.dex.grpc.integration.clients.combined

import cats.syntax.either._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesControlledStream, BlockchainUpdatesConversions}
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.matcherext.{UtxEventConversions, UtxEventsControlledStream}
import com.wavesplatform.dex.meta.getSimpleName
import monix.eval.Task
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import play.api.libs.json.{Format, Reads, Writes}

import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.chaining._

/**
 * During reconnects we need to:
 * 1. Notify that the connection failed and switch the system to the TransientRollback state
 * 2. Recover the UTX stream, so UTX events will be stashed
 * 3. Recover the blockchain updates stream
 */
class CombinedStream(
  settings: CombinedStream.Settings,
  blockchainUpdates: BlockchainUpdatesControlledStream,
  utxEvents: UtxEventsControlledStream
)(implicit scheduler: Scheduler)
    extends ScorexLogging {

  // Note, it is:
  // * limited by 1
  // * could be decreased during rollbacks
  @volatile private var processedHeight = 1

  private val internalStream = ConcurrentSubject.publish[WavesNodeEvent]
  val stream: Observable[WavesNodeEvent] = internalStream

  private val runWithDelay: (() => Unit) => Unit =
    if (settings.restartDelay.length == 0) { f => f() }
    else f => scheduler.scheduleOnce(settings.restartDelay)(f())

  blockchainUpdates.stream
    .foreach { evt =>
      evt.update.flatMap(BlockchainUpdatesConversions.toEvent) match {
        case Some(x) => internalStream.onNext(x)
        case None =>
          log.error(s"Can't convert $evt to a domain event, asking next")
          blockchainUpdates.requestNext()
      }
    }
    .onComplete {
      case Failure(e) => log.error("blockchainUpdates failed", e)
      case _ => log.info("blockchainUpdates completed")
    }

  utxEvents.stream
    .foreach { evt =>
      UtxEventConversions.toEvent(evt).foreach(internalStream.onNext)
    }
    .onComplete {
      case Failure(e) => log.error("utxEvents failed", e)
      case _ => log.info("utxEvents completed")
    }

  private val mergedEvents = ConcurrentSubject.publish[Either[SystemEvent, SystemEvent]]

  @volatile var blockchainStatus: Status = Status.Starting()

  def status(): Status = blockchainStatus

  val lastStatus = mergedEvents
    .foldLeft[Status](Status.Starting()) {
      case (orig, Left(evt)) => utxEventsTransitions(orig, evt).tap { updated =>
          blockchainStatus = updated
          log.info(s"utx: $orig + $evt -> $updated")
        }
      case (orig, Right(evt)) => blockchainEventsTransitions(orig, evt).tap { updated =>
          blockchainStatus = updated
          log.info(s"bu: $orig + $evt -> $updated")
        }
    }
    .doOnComplete(Task(log.info("lastStatus completed")))
    .doOnError(e => Task(log.error("lastStatus failed", e)))
    .lastL
    .runToFuture

  utxEvents.systemStream
    .map(_.asLeft[SystemEvent])
    .subscribe(
      { x =>
        mergedEvents.onNext(x)
        Ack.Continue
      },
      e => log.error("utxEvents system stream failed", e), // Won't happen
      () => log.info("utxEvents system stream completed") // We do this manually in finish()
    )

  blockchainUpdates.systemStream
    .map(_.asRight[SystemEvent])
    .subscribe(
      { x =>
        mergedEvents.onNext(x)
        Ack.Continue
      },
      e => log.error("bu system stream failed", e), // Won't happen
      () => log.info("bu system stream completed") // We do this manually in finish()
    )

  // TODO DEX-1034
  def startFrom(height: Int): Unit = {
    log.info(s"Starting from $height")
    updateProcessedHeight(height - 1) // Consider we processed the previous
    utxEvents.start()
  }

  def updateProcessedHeight(height: Int): Unit = processedHeight = height
  def currentProcessedHeight: Int = processedHeight

  def restart(): Unit = {
    log.info("Restarting")
    // Self-healed,
    // see blockchainEventsTransitions: ? + Stopped
    // then utxEventsTransitions: Stopping + Stopped
    blockchainUpdates.stop()
  }

  private def utxEventsTransitions(origStatus: Status, event: SystemEvent): Status = {
    def ignore(): Status = {
      log.error(s"utx: Unexpected transition $origStatus + $event, ignore")
      origStatus
    }

    origStatus match {
      case origStatus: Status.Starting =>
        event match {
          case SystemEvent.BecameReady =>
            if (origStatus.utxEvents) ignore()
            else if (origStatus.oneDone) Status.Working
            else {
              blockchainUpdates.startFrom(processedHeight + 1)
              origStatus.copy(utxEvents = true)
            }

          case SystemEvent.Stopped =>
            // We don't need to stop blockchain updates, because we start it
            // only after receiving SystemEvent.BecameReady from UTX Stream
            recover()
            Status.Starting()

          case SystemEvent.Closed =>
            blockchainUpdates.close()
            Status.Closing(utxEvents = true)
        }

      case origStatus: Status.Stopping =>
        event match {
          case SystemEvent.BecameReady => ignore()

          case SystemEvent.Stopped =>
            if (origStatus.utxEvents) ignore()
            else if (origStatus.oneDone) {
              recover()
              Status.Starting()
            } else {
              blockchainUpdates.stop()
              origStatus.copy(utxEvents = true)
            }

          case SystemEvent.Closed =>
            blockchainUpdates.close()
            Status.Closing(utxEvents = true)
        }

      case Status.Working =>
        event match {
          case SystemEvent.BecameReady => ignore()

          case SystemEvent.Stopped =>
            // See Starting + Stopped
            blockchainUpdates.stop()
            Status.Stopping(utxEvents = true)

          case SystemEvent.Closed =>
            blockchainUpdates.close()
            Status.Closing(utxEvents = true)
        }

      case origStatus: Status.Closing =>
        event match {
          case SystemEvent.Closed =>
            if (origStatus.utxEvents) ignore()
            else {
              if (origStatus.oneDone) finish()
              origStatus.copy(utxEvents = true)
            }

          case _ => ignore()
        }

      case _ => throw new IllegalArgumentException(s"Can't process unexpected status=$origStatus. It should be member of ${Status.getClass}")
    }
  }

  private def blockchainEventsTransitions(origStatus: Status, event: SystemEvent): Status = {
    def ignore(): Status = {
      log.error(s"bu: Unexpected transition $origStatus + $event, ignore")
      origStatus
    }

    origStatus match {
      case origStatus: Status.Starting =>
        event match {
          case SystemEvent.BecameReady =>
            if (origStatus.blockchainUpdates) ignore()
            else if (origStatus.oneDone) Status.Working
            else origStatus.copy(blockchainUpdates = true)

          case SystemEvent.Stopped =>
            // See utxEventsTransitions: Starting + Stopped
            utxEvents.stop()
            Status.Stopping(blockchainUpdates = true)

          case SystemEvent.Closed =>
            utxEvents.close()
            Status.Closing(blockchainUpdates = true)
        }

      case origStatus: Status.Stopping =>
        event match {
          case SystemEvent.BecameReady => ignore()

          case SystemEvent.Stopped =>
            if (origStatus.blockchainUpdates) ignore()
            else if (origStatus.oneDone) {
              recover()
              Status.Starting()
            } else {
              utxEvents.stop()
              origStatus.copy(blockchainUpdates = true)
            }

          case SystemEvent.Closed =>
            utxEvents.close()
            Status.Closing(blockchainUpdates = true)
        }

      case Status.Working =>
        event match {
          case SystemEvent.BecameReady => ignore()

          case SystemEvent.Stopped =>
            // See utxEventsTransitions: Starting + Stopped
            utxEvents.stop()
            Status.Stopping(blockchainUpdates = true)

          case SystemEvent.Closed =>
            utxEvents.close()
            Status.Closing(blockchainUpdates = true)
        }

      case origStatus: Status.Closing =>
        event match {
          case SystemEvent.Closed =>
            if (origStatus.blockchainUpdates) ignore()
            else {
              if (origStatus.oneDone) finish()
              origStatus.copy(blockchainUpdates = true)
            }

          case _ => ignore()
        }
      case _ => throw new IllegalArgumentException(s"Can't process unexpected status=$origStatus. It should be member of ${Status.getClass}")
    }
  }

  // Recovering after stopped streams
  private def recover(): Unit = {
    // We don't know, did we process the last block. It could be liquid and we could not receive all micro blocks.
    // But we processed the previous block without doubt.
    val updatedProcessedHeight = math.max(1, processedHeight - 1)
    internalStream.onNext(WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.Height(updatedProcessedHeight)))
    // We need to updated this too, because RolledBack could not be processed
    // before we call startFrom in utxEventsTransitions: Starting + BecameReady
    updateProcessedHeight(updatedProcessedHeight)
    runWithDelay(() => utxEvents.start())
  }

  private def finish(): Unit = {
    log.info("Finished")
    internalStream.onComplete()
    mergedEvents.onComplete()
  }

}

object CombinedStream {
  case class Settings(restartDelay: FiniteDuration)

  sealed abstract class Status extends Product with Serializable {
    val name: String = getSimpleName(this)
  }

  object Status {

    final case class Starting(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Stopping(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Closing(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case object Working extends Status

    val All = List(Starting(), Stopping(), Closing(), Working)

    implicit val format: Format[Status] = Format(
      Reads.StringReads.map { x =>
        All.find(_.name == x) match {
          case Some(r) => r
          case None => throw new IllegalArgumentException(s"Can't parse '$x' as CombinedStream.Status")
        }
      },
      Writes.StringWrites.contramap(_.name)
    )

    sealed trait HasStreams {
      def blockchainUpdates: Boolean
      def utxEvents: Boolean

      def oneDone: Boolean = blockchainUpdates || utxEvents
      override def toString: String = s"${getSimpleName(this)}(b=$blockchainUpdates, u=$utxEvents)"
    }

  }

}
