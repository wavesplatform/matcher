package com.wavesplatform.dex.grpc.integration.dto

import com.wavesplatform.common.state.ByteStr

case class BriefAssetDescription(name: ByteStr, decimals: Int, hasScript: Boolean)
