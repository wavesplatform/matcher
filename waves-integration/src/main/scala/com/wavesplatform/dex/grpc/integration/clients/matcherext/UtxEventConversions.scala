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
                    log.info(s"${tx.id.toVanilla} failed: ${reason.name}, ${reason.message}")
                    r
                }
            }
          }

        if (addedTxs.isEmpty && failedTxs.isEmpty) none
        else WavesNodeEvent.UtxUpdated(addedTxs, failedTxs).some
      case _ =>
        log.error(s"Can't convert $event to a domain event")
        none
    }

}
