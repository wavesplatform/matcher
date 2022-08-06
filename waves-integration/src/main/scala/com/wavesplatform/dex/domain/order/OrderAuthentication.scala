package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs

sealed trait OrderAuthentication {
  def updateProofs(proofs: Proofs): OrderAuthentication
  def updateSender(sender: PublicKey): OrderAuthentication
  def updateEip712Signature(signature: ByteStr): OrderAuthentication
}

object OrderAuthentication {

  final case class OrderProofs(key: PublicKey, proofs: Proofs) extends OrderAuthentication {
    override def updateProofs(proofs: Proofs): OrderProofs = copy(proofs = proofs)
    override def updateSender(sender: PublicKey): OrderProofs = copy(key = sender)
    override def updateEip712Signature(signature: ByteStr): OrderProofs = this
  }

  final case class Eip712Signature(signature: ByteStr) extends OrderAuthentication {
    override def updateProofs(proofs: Proofs): Eip712Signature = this
    override def updateSender(sender: PublicKey): Eip712Signature = this
    override def updateEip712Signature(sig: ByteStr): Eip712Signature = copy(signature = sig)
  }

}
