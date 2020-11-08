package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.bytes.ByteStr

case class WavesBlock(ref: BlockRef, reference: ByteStr, changes: BlockchainBalance, tpe: WavesBlock.Type) {

  def diffIndex: DiffIndex = DiffIndex(
    regular = changes.regular.view.mapValues(_.keySet).toMap,
    outLeases = changes.outLeases.keySet
  )

}

object WavesBlock {
  sealed trait Type extends Product with Serializable

  object Type {
    case object Block extends Type
    case object MicroBlock extends Type
  }

}
