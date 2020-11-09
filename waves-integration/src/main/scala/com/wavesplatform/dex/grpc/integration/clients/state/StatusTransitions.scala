package com.wavesplatform.dex.grpc.integration.clients.state

import cats.kernel.Monoid
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainEvent._
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.HeightUpdate

import scala.collection.immutable.Queue

/**
 * The extension guarantees:
 * 1. During initialization: events will be sent after initial blocks
 * 2. Events has causal ordering
 */

object StatusTransitions extends ScorexLogging {

  def apply(origStatus: BlockchainStatus, event: BlockchainEvent): StatusUpdate = {
    val r = origStatus match {
      case origStatus: Normal =>
        event match {
          case Appended(block) =>
            origStatus.mainFork.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                StatusUpdate(
                  TransientRollback(
                    newFork = WavesFork(List.empty),
                    newForkChanges = Monoid.empty[BlockchainBalance],
                    previousForkHeight = origStatus.currentHeightHint,
                    previousForkDiffIndex = origStatus.mainFork.diffIndex
                  ),
                  updatedHeight = HeightUpdate.RestartRequired(math.max(0, origStatus.currentHeightHint - 1))
                )
              case Right(updatedFork) =>
                StatusUpdate(
                  newStatus = Normal(updatedFork, block.ref.height),
                  updatedBalances = block.changes,
                  updatedHeight = if (block.tpe == WavesBlock.Type.Block) HeightUpdate.Updated(block.ref.height) else HeightUpdate.NotChanged
                )
            }

          case RolledBackTo(commonBlockRef) =>
            val (commonFork, droppedDiff) = origStatus.mainFork.dropAfter(commonBlockRef)
            if (commonFork.history.isEmpty)
              StatusUpdate(
                newStatus = TransientResolving(commonFork, Queue.empty, commonBlockRef.height),
                requestBalances = droppedDiff
              )
            else
              StatusUpdate(
                newStatus = TransientRollback(
                  newFork = commonFork,
                  newForkChanges = Monoid.empty[BlockchainBalance],
                  previousForkHeight = commonBlockRef.height,
                  previousForkDiffIndex = droppedDiff
                )
              )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus)
        }

      case origStatus: TransientRollback =>
        event match {
          case Appended(block) =>
            origStatus.newFork.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                StatusUpdate(
                  TransientRollback(
                    newFork = WavesFork(List.empty),
                    newForkChanges = Monoid.empty[BlockchainBalance],
                    previousForkHeight = origStatus.previousForkHeight,
                    previousForkDiffIndex = origStatus.previousForkDiffIndex
                  ),
                  updatedHeight = HeightUpdate.RestartRequired(math.max(1, origStatus.previousForkHeight - 1)) // TODO duplication of max
                )

              case Right(updatedNewFork) =>
                val newForkChanges = origStatus.newForkChanges |+| block.changes
                if (block.tpe == WavesBlock.Type.Block)
                  StatusUpdate(
                    newStatus = origStatus.copy(
                      newFork = updatedNewFork,
                      newForkChanges = newForkChanges
                    )
                    // updatedHeight = updatedHeight // We don't notify about updates until we get the same height
                  )
                else
                  StatusUpdate(
                    newStatus = TransientResolving(
                      mainFork = updatedNewFork,
                      stash = Queue.empty,
                      currentHeightHint = block.ref.height
                    ),
                    updatedBalances = newForkChanges,
                    requestBalances = origStatus.previousForkDiffIndex.without(newForkChanges.diffIndex),
                    updatedHeight = if (block.tpe == WavesBlock.Type.Block) HeightUpdate.Updated(block.ref.height) else HeightUpdate.NotChanged
                  )
            }

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus)
        }

      case origStatus: TransientResolving =>
        event match {
          case DataReceived(updates) =>
            val init = StatusUpdate(
              newStatus = Normal(origStatus.mainFork, origStatus.currentHeightHint),
              updatedBalances = updates
            )

            origStatus.stash.foldLeft(init) {
              case (r, x) => r |+| apply(r.newStatus, x)
            }

          case _ => StatusUpdate(origStatus.copy(stash = origStatus.stash.enqueue(event)))
        }
    }

    log.info(s"${origStatus.name} + $event = $r")
    r
  }

}
