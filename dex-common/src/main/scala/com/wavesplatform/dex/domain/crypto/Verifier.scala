package com.wavesplatform.dex.domain.crypto

import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.utils.ScorexLogging

object Verifier extends ScorexLogging {

  def verifyAsEllipticCurveSignature[T <: Proven with Authorized](pt: T): Either[GenericError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender), pt, GenericError(s"Proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}
