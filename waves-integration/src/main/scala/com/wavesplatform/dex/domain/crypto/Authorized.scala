package com.wavesplatform.dex.domain.crypto

import com.wavesplatform.dex.domain.account.PublicKey

trait Authorized {
  def sender: PublicKey
}
