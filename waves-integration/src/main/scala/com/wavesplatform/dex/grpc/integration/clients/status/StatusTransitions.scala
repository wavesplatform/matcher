package com.wavesplatform.dex.grpc.integration.clients.status

import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.status.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.status.WavesFork.Status
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent.RolledBack.To
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent._

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
                  processUtxEvents = if (forgedTxIds.isEmpty) Queue.empty else Queue(WavesNodeUtxEvent.Forged(forgedTxIds))
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

          case RolledBack(to) => // This could be during appending a new key block too
            StatusUpdate(
              newStatus = TransientRollback(
                fork = to match {
                  case To.CommonBlockRef(ref) => WavesFork.mk(origStatus.main, ref)
                  case To.Height(h) => WavesFork.mk(origStatus.main, h)
                },
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
                    processUtxEvents = updatedUtxEventsStash
                  )
                else
                  StatusUpdate(
                    newStatus = TransientResolving(
                      main = activeBranch,
                      stashChanges = newChanges,
                      stash = Queue.empty,
                      utxEventsStash = updatedUtxEventsStash
                    ),
                    requestBalances = lostDiffIndex,
                    updatedLastBlockHeight = LastBlockHeight.NotChanged,
                    processUtxEvents = Queue.empty
                  )

              case Status.NotResolved(updatedFork) =>
                StatusUpdate(
                  newStatus = TransientRollback(
                    fork = updatedFork,
                    utxEventsStash = updatedUtxEventsStash
                  )
                )

              case Status.Failed(updatedFork, reason) =>
                log.error(s"Forcibly rollback, because of error: $reason")
                StatusUpdate(
                  newStatus = TransientRollback(
                    fork = updatedFork,
                    utxEventsStash = origStatus.utxEventsStash // TODO ??? hm we just dropped a transaction from the last block?
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
            StatusUpdate(newStatus = origStatus.copy(fork = fork))

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
              newStatus = Normal(origStatus.main),
              updatedBalances = origStatus.stashChanges |+| updates,
              processUtxEvents = origStatus.utxEventsStash,
              updatedLastBlockHeight = LastBlockHeight.Updated(origStatus.main.height)
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
