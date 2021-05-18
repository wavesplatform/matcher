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
                StatusUpdate(
                  newStatus = origStatus, // We will wait for RolledBack after RestartRequired
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired
                )

              case Right(updatedChain) =>
                StatusUpdate(
                  newStatus = Normal(updatedChain),
                  updatedBalances = block.changes,
                  updatedLastBlockHeight = LastBlockHeight.Updated(updatedChain.height),
                  utxUpdate = UtxUpdate(confirmedTxs = block.confirmedTxs),
                  requestNextBlockchainEvent = true
                )
            }

          case RolledBack(to) =>
            val initFork = WavesFork(origStatus.main, origStatus.main)
            val updatedFork = to match {
              case To.CommonBlockRef(ref) => initFork.rollbackTo(ref)
              case To.Height(h) => initFork.rollbackTo(h)
            }
            StatusUpdate(
              newStatus = TransientRollback(
                fork = updatedFork,
                utxUpdate = Monoid.empty[UtxUpdate]
              ),
              updatedLastBlockHeight = LastBlockHeight.Updated(updatedFork.height),
              requestNextBlockchainEvent = true
            )

          case UtxUpdated(newTxs, failedTxs) =>
            StatusUpdate(
              newStatus = origStatus,
              utxUpdate = UtxUpdate(unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap, failedTxs = failedTxs.view.map(x => x.id -> x).toMap)
            )

          case UtxSwitched(newTxs) =>
            // Normally won't happen
            StatusUpdate(
              newStatus = origStatus,
              utxUpdate = UtxUpdate(unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap, resetCaches = true)
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
                val finalUtxUpdate = {
                  val x = origStatus.utxUpdate |+| UtxUpdate(
                    confirmedTxs = resolved.newConfirmedTxs,
                    failedTxs = Map.empty // resolved.lostTxIds
                  )

                  // This solves a situation when rolled back transactions are moved to UTX Pool
                  // and then confirmed in a micro block.
                  // Relates DEX-1099
                  x.copy(unconfirmedTxs = x.unconfirmedTxs.filterNot(tx => resolved.commonTxIds.contains(tx._1)))
                }

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
                    updatedLastBlockHeight = LastBlockHeight.Updated(resolved.activeChain.height),
                    requestBalances = resolved.lostDiffIndex
                    // requestNextBlockchainEvent = true // Because we are waiting for DataReceived
                  )

              case Status.NotResolved(updatedFork) =>
                StatusUpdate(
                  newStatus = origStatus.copy(fork = updatedFork),
                  updatedLastBlockHeight = LastBlockHeight.Updated(updatedFork.height),
                  requestNextBlockchainEvent = true
                )

              case Status.Failed(reason) =>
                log.error(s"Forcibly rollback, because of error: $reason")
                StatusUpdate(
                  origStatus, // We will wait for RolledBack after RestartRequired
                  updatedLastBlockHeight = LastBlockHeight.RestartRequired
                )
            }

          case RolledBack(to) =>
            val updatedFork = to match {
              case To.CommonBlockRef(ref) => origStatus.fork.rollbackTo(ref)
              case To.Height(h) => origStatus.fork.rollbackTo(h)
            }
            StatusUpdate(
              newStatus = origStatus.copy(fork = updatedFork),
              updatedLastBlockHeight = LastBlockHeight.Updated(updatedFork.height),
              requestNextBlockchainEvent = true
            )

          case UtxUpdated(newTxs, failedTxs) =>
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = origStatus.utxUpdate |+| UtxUpdate(
                  unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap,
                  failedTxs = failedTxs.view.map(x => x.id -> x).toMap
                )
              )
            )

          case UtxSwitched(newTxs) =>
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = UtxUpdate(unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap, resetCaches = true) // Forget the previous
              )
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
                  unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap,
                  failedTxs = failedTxs.view.map(x => x.id -> x).toMap
                )
              )
            )

          case UtxSwitched(newTxs) =>
            log.error("Unexpected UTxSwitched, reset utxUpdate")
            StatusUpdate(
              newStatus = origStatus.copy(
                utxUpdate = UtxUpdate(unconfirmedTxs = newTxs.map(tx => tx.id -> tx).toMap, resetCaches = true) // Forget the previous
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

    log.trace(s"Result: $r")
    r
  }

}
