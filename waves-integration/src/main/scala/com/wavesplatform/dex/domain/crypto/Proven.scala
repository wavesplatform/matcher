package com.wavesplatform.dex.domain.crypto

import monix.eval.Coeval

trait Proven extends Authorized {

  val bodyBytes: Coeval[Array[Byte]]
  def proofs: Proofs
}
