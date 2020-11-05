package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainStatus.DiffIndex

case class BaseBlock(ref: BlockRef, reference: ByteStr, changes: BlockchainBalance, tpe: BaseBlock.Type) {

  def diffIndex: DiffIndex = DiffIndex(
    regular = changes.regular.view.mapValues(_.keySet).toMap,
    outLeases = changes.outLeases.keySet
  )

}

object BaseBlock {
  sealed trait Type

  object Type {
    case object Block extends Type
    case object MicroBlock extends Type
  }

}
