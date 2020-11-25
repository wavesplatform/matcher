package com.wavesplatform.dex.grpc.integration.clients.status

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent._
import com.wavesplatform.dex.grpc.integration.clients.status.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.status.StatusUpdate.LastBlockHeight

import scala.collection.immutable.Queue

/**
 * The extension guarantees:
 * 1. During initialization: events will be sent after initial blocks
 * 2. Events has causal ordering
 */

object StatusTransitions extends ScorexLogging {

  def apply(origStatus: BlockchainStatus, event: WavesNodeEvent): StatusUpdate = {
    val r = origStatus match {
      case origStatus: Normal =>
        event match {
          case Appended(block, forgedTxIds) =>
            val utxEventsStash = Queue(WavesNodeUtxEvent.Forged(forgedTxIds))
            origStatus.mainFork.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                val previousBlock = origStatus.mainFork.history.tail.headOption
                val (newFork, _) = previousBlock match {
                  case Some(previousBlock) => origStatus.mainFork.dropAfter(previousBlock.ref)
                  case None => origStatus.mainFork.dropAll
                }

                StatusUpdate(
                  newStatus = TransientRollback(
                    newFork = newFork,
                    newForkChanges = Monoid.empty[BlockchainBalance],
                    previousForkHeight = origStatus.currentHeightHint,
                    previousForkDiffIndex = origStatus.mainFork.diffIndex,
                    utxEventsStash = utxEventsStash
                  ),
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired(math.max(1, origStatus.currentHeightHint - 1))
                )
              case Right(updatedFork) =>
                StatusUpdate(
                  newStatus = Normal(updatedFork, block.ref.height),
                  updatedBalances = block.changes,
                  updatedLastBlockHeight =
                    if (block.tpe == WavesBlock.Type.Block) LastBlockHeight.Updated(block.ref.height) else LastBlockHeight.NotChanged,
                  processUtxEvents = utxEventsStash
                )
            }

          case UtxAdded(txs) =>
            StatusUpdate(
              newStatus = origStatus,
              processUtxEvents = Queue(WavesNodeUtxEvent.Added(txs))
            )

          case UtxSwitched(newTxs) =>
            StatusUpdate(
              newStatus = origStatus,
              processUtxEvents = Queue(WavesNodeUtxEvent.Switched(newTxs))
            )

          case RolledBackTo(commonBlockRef) =>
            val (commonFork, droppedDiff) = origStatus.mainFork.dropAfter(commonBlockRef)
            StatusUpdate(
              newStatus = TransientRollback(
                newFork = commonFork,
                newForkChanges = Monoid.empty[BlockchainBalance],
                previousForkHeight = origStatus.currentHeightHint,
                previousForkDiffIndex = droppedDiff,
                utxEventsStash = Queue.empty
              )
            )

          case SyncFailed(height) =>
            val (commonFork, droppedDiff) = origStatus.mainFork.dropFrom(height)
            StatusUpdate(
              newStatus = TransientRollback(
                newFork = commonFork,
                newForkChanges = Monoid.empty[BlockchainBalance],
                previousForkHeight = origStatus.currentHeightHint,
                previousForkDiffIndex = droppedDiff,
                utxEventsStash = Queue.empty
              )
            )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus)
        }

      case origStatus: TransientRollback =>
        event match {
          case Appended(block, forgedTxIds) =>
            val updatedUtxEventsStash = origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Forged(forgedTxIds))
            origStatus.newFork.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                StatusUpdate(
                  TransientRollback(
                    newFork = WavesFork(List.empty),
                    newForkChanges = Monoid.empty[BlockchainBalance],
                    previousForkHeight = origStatus.previousForkHeight,
                    previousForkDiffIndex = origStatus.previousForkDiffIndex,
                    utxEventsStash = updatedUtxEventsStash
                  ),
                  updatedLastBlockHeight =
                    LastBlockHeight.RestartRequired(math.max(1, origStatus.previousForkHeight - 1)) // TODO duplication of max
                )

              case Right(updatedNewFork) =>
                val newForkChanges = origStatus.newForkChanges |+| block.changes
                if (block.tpe == WavesBlock.Type.Block)
                  StatusUpdate(
                    newStatus = origStatus.copy(
                      newFork = updatedNewFork,
                      newForkChanges = newForkChanges,
                      utxEventsStash = updatedUtxEventsStash
                    )
                    // updatedHeight = updatedHeight // We don't notify about updates until we get the same height
                  )
                else {
                  // We don't a height, because a micro block comes after all blocks
                  val requestBalances = origStatus.previousForkDiffIndex.without(newForkChanges.diffIndex) // TODO
                  val (newStatus, updatedProcessUtxEvents) =
                    if (requestBalances.isEmpty) Normal(
                      mainFork = updatedNewFork,
                      currentHeightHint = block.ref.height
                    ) -> updatedUtxEventsStash
                    else TransientResolving(
                      mainFork = updatedNewFork,
                      stash = Queue.empty,
                      currentHeightHint = block.ref.height,
                      utxEventsStash = updatedUtxEventsStash
                    ) -> Queue.empty
                  StatusUpdate(
                    newStatus = newStatus,
                    updatedBalances = newForkChanges,
                    requestBalances = requestBalances,
                    // TODO Do we really need this?
                    updatedLastBlockHeight =
                      if (block.tpe == WavesBlock.Type.Block) LastBlockHeight.Updated(block.ref.height) else LastBlockHeight.NotChanged,
                    processUtxEvents = updatedProcessUtxEvents
                  )
                }
            }

          case UtxAdded(txs) =>
            StatusUpdate(newStatus = origStatus.copy(utxEventsStash = origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Added(txs))))

          case UtxSwitched(newTxs) =>
            StatusUpdate(newStatus = origStatus.copy(utxEventsStash = origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Switched(newTxs))))

          case RolledBackTo(commonBlockRef) =>
            val (commonFork, droppedDiff) = origStatus.newFork.dropAfter(commonBlockRef)
            StatusUpdate(
              newStatus = TransientRollback(
                newFork = commonFork,
                newForkChanges = Monoid.empty[BlockchainBalance],
                previousForkHeight = origStatus.previousForkHeight,
                previousForkDiffIndex = origStatus.previousForkDiffIndex |+| droppedDiff,
                utxEventsStash = origStatus.utxEventsStash
              )
            )

          case SyncFailed(height) =>
            val (commonFork, droppedDiff) = origStatus.newFork.dropFrom(height)
            StatusUpdate(
              newStatus = TransientRollback(
                newFork = commonFork,
                newForkChanges = Monoid.empty[BlockchainBalance],
                previousForkHeight = origStatus.previousForkHeight,
                previousForkDiffIndex = origStatus.previousForkDiffIndex |+| droppedDiff,
                utxEventsStash = origStatus.utxEventsStash
              )
            )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus)
        }

      case origStatus: TransientResolving =>
        event match {
          // TODO We can stuck if waiting for DataReceiving in stash, because we don't send a request !!!!
          // OR not, because we are creating a new stash
          // Also we need a some protection from stuck!
          case DataReceived(updates) =>
            // TODO optimize. Probably we don't need to request all data. E.g. we hadn't this address in last 100 blocks and we got its balance 101 block before
            val init = StatusUpdate(
              newStatus = Normal(origStatus.mainFork, origStatus.currentHeightHint),
              updatedBalances = updates,
              processUtxEvents = origStatus.utxEventsStash
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
