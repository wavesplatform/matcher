package com.wavesplatform.dex.model

import com.wavesplatform.common.state.ByteStr

case class BriefAssetDescription(name: ByteStr, decimals: Int, hasScript: Boolean)
