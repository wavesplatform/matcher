package com.wavesplatform.dex.it.config.genesis

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr

case class SignerData(generator: PublicKey, signature: ByteStr)
