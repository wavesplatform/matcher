package com.wavesplatform.dex.grpc.integration.clients.domain

import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.bytes.ByteStr

case class WavesBlock(
  ref: BlockRef,
  reference: ByteStr,
  changes: BlockchainBalance,
  tpe: WavesBlock.Type,
  confirmedTxs: Map[ByteString, TransactionWithChanges]
) {

  def diffIndex: DiffIndex = DiffIndex(
    regular = changes.regular.view.mapValues(_.keySet).toMap,
    outgoingLeasing = changes.outgoingLeasing.keySet
  )

  override def toString: String = s"B($ref, tpe=$tpe, c=${confirmedTxs.size})"

}

object WavesBlock {
  sealed trait Type extends Product with Serializable

  object Type {

    case object FullBlock extends Type {
      override val toString = "f"
    }

    case object MicroBlock extends Type {
      override val toString = "m"
    }

  }

}
