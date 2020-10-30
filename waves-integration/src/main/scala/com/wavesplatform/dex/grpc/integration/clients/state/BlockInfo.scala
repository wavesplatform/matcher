package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.bytes.ByteStr

case class BlockInfo(height: Int, id: ByteStr)

object BlockInfo {

  def compare(a: BlockInfo, b: BlockInfo) =
    if (a.height == b.height)
      if (a.id == b.id) orig.asRight
      else s"Unexpected signature: ${a.id} != ${b.id}".asLeft

}
