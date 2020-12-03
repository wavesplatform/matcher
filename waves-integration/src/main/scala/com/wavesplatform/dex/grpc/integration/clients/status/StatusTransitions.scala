package com.wavesplatform.dex.grpc.integration.clients.status

import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.status.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.status.WavesFork.Status
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent.RolledBack.To
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent._

import scala.collection.immutable.Queue

object StatusTransitions extends ScorexLogging {

  def apply(origStatus: BlockchainStatus, event: WavesNodeEvent): StatusUpdate = {
    log.info(s"${origStatus.name} + $event")
    val r = origStatus match {
      case origStatus: Normal =>
        event match {
          case Appended(block, forgedTxIds) =>
            origStatus.main.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                val fork = WavesFork.mkRolledBackByOne(origStatus.main)
                StatusUpdate(
                  newStatus = TransientRollback(fork, Queue.empty),
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired(fork.height + 1)
                )

              case Right(updatedFork) =>
                StatusUpdate(
                  newStatus = Normal(updatedFork),
                  updatedBalances = block.changes,
                  updatedLastBlockHeight =
                    if (block.tpe == WavesBlock.Type.FullBlock) LastBlockHeight.Updated(updatedFork.height)
                    else LastBlockHeight.NotChanged,
                  processUtxEvents = if (forgedTxIds.isEmpty) Queue.empty else Queue(WavesNodeUtxEvent.Forged(forgedTxIds)),
                  requestNextBlockchainEvent = true
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

          case RolledBack(to) => // This could happen during an appending of a new key block too
            StatusUpdate(
              newStatus = TransientRollback(
                fork = to match {
                  case To.CommonBlockRef(ref) => WavesFork.mk(origStatus.main, ref)
                  case To.Height(h) => WavesFork.mk(origStatus.main, h)
                },
                utxEventsStash = Queue.empty
              ),
              requestNextBlockchainEvent = true
            )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus, requestNextBlockchainEvent = true)
        }

      case origStatus: TransientRollback =>
        event match {
          case Appended(block, forgedTxIds) =>
            val updatedUtxEventsStash =
              if (forgedTxIds.isEmpty) Queue.empty
              else origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Forged(forgedTxIds))

            origStatus.fork.withBlock(block) match {
              case Status.Resolved(activeBranch, newChanges, lostDiffIndex) =>
                if (lostDiffIndex.isEmpty)
                  StatusUpdate(
                    newStatus = Normal(activeBranch),
                    updatedBalances = newChanges,
                    updatedLastBlockHeight = LastBlockHeight.Updated(activeBranch.height),
                    processUtxEvents = updatedUtxEventsStash,
                    requestNextBlockchainEvent = true
                  )
                else
                  StatusUpdate(
                    newStatus = TransientResolving(
                      main = activeBranch,
                      stashChanges = newChanges,
                      utxEventsStash = updatedUtxEventsStash
                    ),
                    requestBalances = lostDiffIndex,
                    updatedLastBlockHeight = LastBlockHeight.NotChanged,
                    processUtxEvents = Queue.empty,
                    // requestNextBlockchainEvent = true // Because we are waiting for DataReceived
                  )

              case Status.NotResolved(updatedFork) =>
                StatusUpdate(
                  newStatus = TransientRollback(
                    fork = updatedFork,
                    utxEventsStash = updatedUtxEventsStash
                  ),
                  requestNextBlockchainEvent = true
                )

              case Status.Failed(updatedFork, reason) =>
                log.error(s"Forcibly rollback, because of error: $reason")
                StatusUpdate(
                  newStatus = TransientRollback(
                    fork = updatedFork,
                    utxEventsStash = origStatus.utxEventsStash // TODO DEX-1004 Hm we just dropped a transaction from the last block?
                  ),
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired(updatedFork.height + 1)
                )
            }

          case UtxAdded(txs) =>
            StatusUpdate(newStatus = origStatus.copy(utxEventsStash = origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Added(txs))))

          case UtxSwitched(newTxs) =>
            StatusUpdate(newStatus = origStatus.copy(utxEventsStash = origStatus.utxEventsStash.enqueue(WavesNodeUtxEvent.Switched(newTxs))))

          case RolledBack(to) =>
            val fork = to match {
              case To.CommonBlockRef(ref) => origStatus.fork.rollBackTo(ref)
              case To.Height(h) => origStatus.fork.rollBackTo(h)
            }
            StatusUpdate(
              newStatus = origStatus.copy(fork = fork), // TODO DEX-1004 Not only update a fork ?
              requestNextBlockchainEvent = true
            )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(origStatus, requestNextBlockchainEvent = true)
        }

      case origStatus: TransientResolving =>
        event match {
          case DataReceived(updates) =>
            StatusUpdate(
              newStatus = Normal(origStatus.main),
              updatedBalances = origStatus.stashChanges |+| updates,
              processUtxEvents = origStatus.utxEventsStash,
              updatedLastBlockHeight = LastBlockHeight.Updated(origStatus.main.height),
              requestNextBlockchainEvent = true
            )

          case _ =>
            // Won't happen
            log.error("Unexpected transition, ignore")
            StatusUpdate(
              newStatus = origStatus,
              requestNextBlockchainEvent = event match {
                case _: UtxAdded | _: UtxSwitched => false
                case _ => true // If this really happen, we eventually fail to append a new block
              }
            )
        }
    }

    log.info(s"Result: $r")
    r
  }

}
