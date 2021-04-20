package com.wavesplatform.dex.grpc.integration.clients.matcherext

import cats.syntax.option._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxEvent

object UtxEventConversions extends ScorexLogging {

  def toEvent(event: UtxEvent): Option[WavesNodeEvent] =
    event.`type` match {
      case UtxEvent.Type.Switch(event) => WavesNodeEvent.UtxSwitched(event.transactions).some
      case UtxEvent.Type.Update(event) =>
        val addedTxs = event.added.flatMap(_.transaction)
        val failedTxs = event.removed
          .flatMap { tx =>
            tx.reason match {
              case None => none // Because we remove them during adding a full/micro block
              case Some(reason) =>
                tx.transaction match {
                  case None => none
                  case r @ Some(tx) =>
                    if (isFalsePositive(reason)) {
                      // We store txs in the PessimisticPortfolio (PP). If a tx fails in the UTX Pool, we remove it from the PP.
                      // This causes balance changes. Usually, it is okay. Another reason to remove a tx from the PP is when it confirmed.
                      //
                      // But here is another case:
                      // UTX Pool has a supposed state of NODE after confirming all transactions.
                      // There is a race condition on NODE. A tx is confirmed, but not removed from the UTX Pool.
                      // This affects the account balance in a supposed state.
                      // So NODE removes a transaction from UTX with a false-positive reason.
                      //
                      // We ignore such errors to fight this.
                      log.info(s"${tx.id.toVanilla} failed by a false-positive reason: ${reason.name}, ${reason.message}")
                      none
                    } else {
                      log.info(s"${tx.id.toVanilla} failed: ${reason.name}, ${reason.message}")
                      r
                    }
                }
            }
          }

        if (addedTxs.isEmpty && failedTxs.isEmpty) none
        else WavesNodeEvent.UtxUpdated(addedTxs, failedTxs).some
      case _ =>
        log.error(s"Can't convert $event to a domain event")
        none
    }

  // DEX-1120
  private def isFalsePositive(e: UtxEvent.Update.Removed.Reason): Boolean =
    // See WavesBlockchainApiGrpcService.canRetry
    e.name == "OrderValidationError" && e.message.startsWith("Too much")

}
