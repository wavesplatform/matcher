package com.wavesplatform.dex.grpc.integration.clients.combined

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer}
import akka.actor.typed.scaladsl.BehaviorsImplicits._
import com.wavesplatform.dex.actors.CustomLoggerBehaviorInterceptor
import com.wavesplatform.dex.fp.PartialFunctionOps
import com.wavesplatform.dex.fp.PartialFunctionOps.Implicits
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesControlledStream, BlockchainUpdatesConversions}
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.{Settings, Status}
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.matcherext.{UtxEventConversions, UtxEventsControlledStream}
import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject
import org.slf4j.event.Level

import java.util.concurrent.atomic.AtomicInteger

object CombinedStreamActor {
  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Start(fromHeight: Int) extends Command

    case class UpdateProcessedHeight(height: Int) extends Command

    case object Restart extends Command

    case object Continue extends Command

    case class ProcessUtxSystemEvent(evt: SystemEvent) extends Command
    case class ProcessBlockchainUpdatesSystemEvent(evt: SystemEvent) extends Command

    case class ProcessUtxEvent(evt: UtxEvent) extends Command
    case class ProcessBlockchainUpdatesEvent(evt: SubscribeEvent) extends Command
  }

  def apply(
    settings: Settings,
    processedHeight: AtomicInteger,
    blockchainUpdates: BlockchainUpdatesControlledStream,
    utxEvents: UtxEventsControlledStream,
    status: ConcurrentSubject[Status, Status],
    output: ConcurrentSubject[WavesNodeEvent, WavesNodeEvent]
  )(implicit monixScheduler: Scheduler): Behavior[Command] = Behaviors.setup[Command] { context =>
    // Subscribe on events

    blockchainUpdates.systemStream.foreach(context.self ! Command.ProcessBlockchainUpdatesSystemEvent(_))
    blockchainUpdates.stream.foreach(context.self ! Command.ProcessBlockchainUpdatesEvent(_))

    utxEvents.systemStream.foreach(context.self ! Command.ProcessUtxSystemEvent(_))
    utxEvents.stream.foreach(context.self ! Command.ProcessUtxEvent(_))

    // Utilities

    val ignoreAndKeepBehavior: Command => Behavior[Command] = _ => Behaviors.same
    val mkPartial = PartialFunctionOps.mkPartial[Command, Behavior[Command]] _

    def logAndKeepBehavior(text: String): Behavior[Command] = {
      context.log.warn(text)
      Behaviors.same[Command]
    }

    // Possible transitions:
    //   initial -> starting
    //   starting -> working, waitForContinue, stopping, closing
    //   working -> stopping, closing
    //   waitForContinue -> starting, closing
    //   stopping -> waitForContinue, closing
    //   closing -> closed

    def closed: Behavior[Command] = {
      context.log.info("Status: closed")
      status.onNext(Status.Closing(blockchainUpdates = true, utxEvents = true))
      Behaviors.receiveMessage[Command](x => logAndKeepBehavior(s"Unexpected $x"))
    }

    def closing(utxEventsClosed: Boolean, blockchainUpdatesClosed: Boolean): Behavior[Command] = {
      context.log.info(s"Status: closing(utx=$utxEventsClosed, bu=$blockchainUpdatesClosed)")
      status.onNext(Status.Closing(blockchainUpdates = blockchainUpdatesClosed, utxEvents = utxEventsClosed))
      Behaviors.receiveMessage[Command] {
        case Command.ProcessUtxSystemEvent(SystemEvent.Closed) =>
          if (utxEventsClosed)
            // We don't need to call blockchainUpdates.close() here, because utxEventsClosed=true means that
            // we have already received Command.ProcessUtxSystemEvent(SystemEvent.Closed),
            // thus we already called it.
            logAndKeepBehavior("utx has already closed")
          else if (blockchainUpdatesClosed) closed
          else {
            blockchainUpdates.close()
            closing(utxEventsClosed = true, blockchainUpdatesClosed = false)
          }

        case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.Closed) =>
          if (blockchainUpdatesClosed)
            // We should only log the suspicious behavior, see Command.ProcessUtxSystemEvent(SystemEvent.Closed) above.
            logAndKeepBehavior("bu has already closed")
          else if (utxEventsClosed) closed
          else {
            utxEvents.close()
            closing(utxEventsClosed = false, blockchainUpdatesClosed = true)
          }

        case _ => // Silently ignore, it's ok
          Behaviors.same
      }
    }

    val onClosedOrRestart: PartialFunction[Command, Behavior[Command]] = {
      case Command.ProcessUtxSystemEvent(SystemEvent.Closed) =>
        blockchainUpdates.close()
        closing(utxEventsClosed = true, blockchainUpdatesClosed = false)

      case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.Closed) =>
        utxEvents.close()
        closing(utxEventsClosed = false, blockchainUpdatesClosed = true)

      case Command.Restart =>
        blockchainUpdates.stop() // Will be self-healed
        Behaviors.same
    }

    def waitForContinue: Behavior[Command] = Behaviors.receiveMessage[Command] {
      mkPartial {
        case Command.Continue =>
          utxEvents.start()
          starting(utxEventsStarted = false, blockchainUpdatesStarted = false)
      }
        .orElse(onClosedOrRestart)
        .toTotal(ignoreAndKeepBehavior)
    }

    def scheduleAndWaitForContinue(): Behavior[Command] = {
      context.scheduleOnce(settings.restartDelay, context.self, Command.Continue)
      waitForContinue
    }

    def startWithRollback(): Behavior[Command] = {
      context.log.info("Status: startWithRollback")
      output.onNext(WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.Height(processedHeight.decrementAndGet())))
      scheduleAndWaitForContinue()
    }

    def startWithoutRollback(): Behavior[Command] = {
      context.log.info("Status: startWithoutRollback")
      scheduleAndWaitForContinue()
    }

    def starting(utxEventsStarted: Boolean, blockchainUpdatesStarted: Boolean): Behavior[Command] = {
      context.log.info(s"Status: starting(utx=$utxEventsStarted, bu=$blockchainUpdatesStarted)")
      status.onNext(Status.Starting(blockchainUpdates = blockchainUpdatesStarted, utxEvents = utxEventsStarted))
      Behaviors.stashWithCtxPropagation[Command](Int.MaxValue) { stash =>
        Behaviors.receiveMessagePartial[Command] {
          mkPartial {
            case Command.ProcessUtxSystemEvent(SystemEvent.BecameReady) =>
              if (utxEventsStarted)
                // We should only log the suspicious behavior, see closing:Command.ProcessUtxSystemEvent(SystemEvent.Closed) above.
                logAndKeepBehavior("utx has already started")
              else if (blockchainUpdatesStarted) {
                context.log.warn("Unexpected state: bu has already started")
                becomeWorking(stash)
              } else {
                blockchainUpdates.startFrom(processedHeight.get() + 1)
                starting(utxEventsStarted = true, blockchainUpdatesStarted)
              }

            case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.BecameReady) =>
              if (blockchainUpdatesStarted)
                // We should only log the suspicious behavior, see closing:Command.ProcessUtxSystemEvent(SystemEvent.Closed) above.
                logAndKeepBehavior("bu has already started")
              else if (utxEventsStarted) becomeWorking(stash)
              else starting(utxEventsStarted, blockchainUpdatesStarted = true)

            case Command.ProcessUtxSystemEvent(SystemEvent.Stopped) =>
              // We don't need to stop blockchain updates, because we start it
              // only after receiving SystemEvent.BecameReady from UTX Stream
              stash.clear()
              startWithoutRollback()

            case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.Stopped) =>
              utxEvents.stop()
              stopping(utxEventsStopped = false, blockchainUpdatesStopped = true)

            case cmd @ (Command.ProcessUtxEvent(_) | Command.ProcessBlockchainUpdatesEvent(_)) =>
              stash.stash(cmd)
              Behaviors.same
          }.orElse(onClosedOrRestart)
        }
      }
    }

    def working: Behavior[Command] = {
      context.log.info("Status: working")
      status.onNext(Status.Working(processedHeight.get()))
      Behaviors.receiveMessagePartial[Command] {
        mkPartial {
          case Command.ProcessUtxSystemEvent(SystemEvent.Stopped) =>
            // See Starting + Stopped
            blockchainUpdates.stop()
            stopping(utxEventsStopped = true, blockchainUpdatesStopped = false)

          case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.Stopped) =>
            // See utxEventsTransitions: Starting + Stopped
            utxEvents.stop()
            stopping(utxEventsStopped = false, blockchainUpdatesStopped = true)

          case Command.ProcessUtxEvent(evt) =>
            UtxEventConversions.toEvent(evt).foreach(output.onNext) // We log errors in the deep of toEvent
            Behaviors.same

          case Command.ProcessBlockchainUpdatesEvent(evt) =>
            evt.update.flatMap(BlockchainUpdatesConversions.toEvent) match {
              case Some(evt) =>
                output.onNext(evt)
                Behaviors.same
              case None =>
                blockchainUpdates.requestNext()
                logAndKeepBehavior(s"Can't convert $evt to a domain event, asking next")
            }

          case Command.UpdateProcessedHeight(x) =>
            processedHeight.set(x)
            status.onNext(Status.Working(processedHeight.get()))
            Behaviors.same
        }.orElse(onClosedOrRestart)
      }
    }

    def becomeWorking(stash: StashBuffer[Command]): Behavior[Command] = stash.unstashAll(working)

    def stopping(utxEventsStopped: Boolean, blockchainUpdatesStopped: Boolean): Behavior[Command] = {
      context.log.info(s"Status: stopping(utx=$utxEventsStopped, bu=$blockchainUpdatesStopped)")
      status.onNext(Status.Stopping(blockchainUpdates = blockchainUpdatesStopped, utxEvents = utxEventsStopped))
      Behaviors.receiveMessagePartial[Command] {
        mkPartial {
          case Command.ProcessUtxSystemEvent(SystemEvent.Stopped) =>
            if (utxEventsStopped)
              // We should only log the suspicious behavior, see closing:Command.ProcessUtxSystemEvent(SystemEvent.Closed) above.
              logAndKeepBehavior("utx has already stopped")
            else if (blockchainUpdatesStopped) startWithRollback()
            else {
              blockchainUpdates.stop()
              stopping(utxEventsStopped = true, blockchainUpdatesStopped)
            }

          case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.Stopped) =>
            if (blockchainUpdatesStopped) logAndKeepBehavior("bu has already stopped")
            else if (utxEventsStopped)
              // We should only log the suspicious behavior, see closing:Command.ProcessUtxSystemEvent(SystemEvent.Closed) above.
              startWithRollback()
            else {
              utxEvents.stop()
              stopping(utxEventsStopped = false, blockchainUpdatesStopped = true)
            }

          case Command.ProcessUtxSystemEvent(SystemEvent.BecameReady) =>
            utxEvents.stop()
            logAndKeepBehavior("Unexpected event, stopping utx forcefully")

          case Command.ProcessBlockchainUpdatesSystemEvent(SystemEvent.BecameReady) =>
            blockchainUpdates.stop()
            logAndKeepBehavior("Unexpected event, stopping bu forcefully")

          case _: Command.ProcessUtxEvent | _: Command.ProcessBlockchainUpdatesEvent => // Silently ignore, it's ok
            Behaviors.same
        }.orElse(onClosedOrRestart)
      }
    }

    Behaviors.intercept(() =>
      new CustomLoggerBehaviorInterceptor[Command](CustomLoggerBehaviorInterceptor.Settings(
        logger = context.log,
        enabled = true,
        level = Level.DEBUG,
        formatter = {
          case x @ (_: Command.Start | _: Command.UpdateProcessedHeight | Command.Restart | Command.Continue | _: Command.ProcessUtxSystemEvent |
              _: Command.ProcessBlockchainUpdatesSystemEvent) => x.toString
        }
      ))
    ) {
      // Initial
      Behaviors
        .receiveMessagePartial[Command] {
          case Command.Start(fromHeight) =>
            utxEvents.start()
            processedHeight.set(fromHeight - 1)
            starting(utxEventsStarted = false, blockchainUpdatesStarted = false)
        }
        .behavior
    }
  }

}
