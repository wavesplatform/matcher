package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesFork.Status
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent.RolledBack.To
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent._
import com.wavesplatform.dex.meta.getSimpleName

object StatusTransitions extends ScorexLogging {

  def apply(origStatus: BlockchainStatus, event: WavesNodeEvent): StatusUpdate = {
    log.info(s"${origStatus.name} + $event")
    val r = origStatus match {
      case origStatus: Normal =>
        event match {
          case Appended(block) =>
            origStatus.main.withBlock(block) match {
              case Left(e) =>
                log.error(s"Forcibly rollback, because of error: $e")
                val fork = WavesFork.mkRolledBackByOne(origStatus.main)
                StatusUpdate(
                  newStatus = TransientRollback(fork, Monoid.empty[UtxUpdate]),
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired(fork.height + 1)
                )

              case Right(updatedFork) =>
                StatusUpdate(
                  newStatus = Normal(updatedFork),
                  updatedBalances = block.changes,
                  updatedLastBlockHeight =
                    if (block.tpe == WavesBlock.Type.FullBlock) LastBlockHeight.Updated(updatedFork.height)
                    else LastBlockHeight.NotChanged,
                  utxUpdate = UtxUpdate(confirmedTxs = block.forgedTxs),
                  requestNextBlockchainEvent = true
                )
            }

          case UtxUpdated(newTxs, failedTxs) =>
            StatusUpdate(
              newStatus = origStatus,
              utxUpdate = UtxUpdate(unconfirmedTxs = newTxs, failedTxs = failedTxs.view.map(x => x.id -> x).toMap)
            )

          case UtxSwitched(newTxs) =>
            // Normally won't happen
            StatusUpdate(
              newStatus = origStatus,
              utxUpdate = UtxUpdate(unconfirmedTxs = newTxs, resetCaches = true)
            )

          case RolledBack(to) => // This could happen during an appending of a new key block too
            StatusUpdate(
              newStatus = TransientRollback(
                fork = to match {
                  case To.CommonBlockRef(ref) => WavesFork.mk(origStatus.main, ref)
                  case To.Height(h) => WavesFork.mk(origStatus.main, h)
                },
                utxUpdate = Monoid.empty[UtxUpdate]
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
          case Appended(block) =>
            origStatus.fork.withBlock(block) match {
              case resolved: Status.Resolved =>
                val finalUtxUpdate = origStatus.utxUpdate |+| UtxUpdate(
                  confirmedTxs = resolved.forgedTxs,
                  failedTxs = Map.empty // resolved.lostTxIds
                )

                if (resolved.lostDiffIndex.isEmpty)
                  StatusUpdate(
                    newStatus = Normal(resolved.activeChain),
                    updatedBalances = resolved.newChanges,
                    updatedLastBlockHeight = LastBlockHeight.Updated(resolved.activeChain.height),
                    utxUpdate = finalUtxUpdate,
                    requestNextBlockchainEvent = true
                  )
                else
                  StatusUpdate(
                    newStatus = TransientResolving(
                      main = resolved.activeChain,
                      stashChanges = resolved.newChanges,
                      utxUpdate = finalUtxUpdate
                    ),
                    requestBalances = resolved.lostDiffIndex,
                    updatedLastBlockHeight = LastBlockHeight.NotChanged
                    // requestNextBlockchainEvent = true // Because we are waiting for DataReceived
                  )

              case Status.NotResolved(updatedFork) =>
                StatusUpdate(
                  newStatus = origStatus.copy(fork = updatedFork),
                  requestNextBlockchainEvent = true
                )

              case Status.Failed(updatedFork, reason) =>
                log.error(s"Forcibly rollback, because of error: $reason")
                StatusUpdate(
                  origStatus.copy(fork = updatedFork),
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired(updatedFork.height + 1)
                )
            }

          case UtxUpdated(newTxs, failedTxs) =>
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = origStatus.utxUpdate |+| UtxUpdate(
                  unconfirmedTxs = newTxs,
                  failedTxs = failedTxs.view.map(x => x.id -> x).toMap
                )
              )
            )

          case UtxSwitched(newTxs) =>
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = UtxUpdate(unconfirmedTxs = newTxs, resetCaches = true) // Forget the previous
              )
            )

          case RolledBack(to) =>
            val fork = to match {
              case To.CommonBlockRef(ref) => origStatus.fork.rollbackTo(ref)
              case To.Height(h) => origStatus.fork.rollbackTo(h)
            }
            StatusUpdate(
              newStatus = origStatus.copy(fork = fork),
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
              utxUpdate = origStatus.utxUpdate,
              updatedLastBlockHeight = LastBlockHeight.Updated(origStatus.main.height),
              requestNextBlockchainEvent = true
            )

          case UtxUpdated(newTxs, failedTxs) =>
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = origStatus.utxUpdate |+| UtxUpdate(
                  unconfirmedTxs = newTxs,
                  failedTxs = failedTxs.view.map(x => x.id -> x).toMap
                )
              )
            )

          case UtxSwitched(newTxs) =>
            log.error("Unexpected UTxSwitched, reset utxUpdate")
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = UtxUpdate(unconfirmedTxs = newTxs, resetCaches = true) // Forget the previous
              )
            )

          case _ =>
            // Won't happen
            log.error(s"Unexpected ${getSimpleName(event)}, ignore")
            StatusUpdate(
              newStatus = origStatus,
              requestNextBlockchainEvent = true // If this really happen, we eventually fail to append a new block
            )
        }
    }

    log.info(s"Result: $r")
    r
  }

}
