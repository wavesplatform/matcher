package com.wavesplatform.dex.grpc.integration.clients.matcherext

import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.grpc.integration.clients.{isExchangeTransactionFromMatcher => isSignedTransactionFromMatcher}
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, UtxTransaction}

object UtxEventConversions extends ScorexLogging {

  def toEvent(event: UtxEvent): Option[WavesNodeEvent] = {
    event.`type` match {
      case UtxEvent.Type.Switch(event) => WavesNodeEvent.UtxSwitched(event.transactions.filter(predicate)).some
      case UtxEvent.Type.Update(event) =>
        val addedTxs = event.added.collect { case UtxEvent.Update.Added(Some(tx)) if predicate(tx) => tx }
        val failedTxs = event.removed
          .filter(_.transaction.exists(predicate))
          .flatMap { tx =>
            tx.reason match {
              case None => none // Because we remove them during adding a full/micro block
              case Some(reason) =>
                tx.transaction.tapEach { tx =>
                  log.info(s"${tx.id.toVanilla} failed: ${reason.name}, ${reason.message}")
                }
            }
          }

        if (addedTxs.isEmpty && failedTxs.isEmpty) none
        else WavesNodeEvent.UtxUpdated(addedTxs, failedTxs).some
      case _ => none
    }
  }

}
