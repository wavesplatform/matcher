package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.bytes.ByteStr

case class BlockRef(height: Int, id: ByteStr)
