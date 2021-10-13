package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.{Order, OrderV3}

import java.io.ByteArrayOutputStream

object ProofsLpHack {

  // HACK for LP

  // see com.wavesplatform.lang.utils.Serialize.ByteArrayOutputStreamOps
  private def encodeToBytes(n: Long, byteCount: Int): ByteStr = {
    val s = new ByteArrayOutputStream(byteCount)
    (byteCount - 1 to 0 by -1).foreach { i =>
      s.write((n >> (8 * i) & 0xffL).toInt)
    }
    ByteStr(s.toByteArray)
  }

  def updateProofs(proofs: Proofs, executedAmount: Long, executedPrice: Long): Proofs =
    proofs.proofs ++ List(encodeToBytes(executedAmount, 8), encodeToBytes(executedPrice, 8))

  def fillMatchInfoInProofs(order: Order, executedAmount: Long, executedPrice: Long): Order =
    order match {
      case order: OrderV3 => order.copy(proofs = updateProofs(order.proofs, executedAmount, executedPrice))
      case _ => order // HACK
    }

  def fillMatchInfoInProofs(order: Order, executedAmount: Long, executedPrice: Long, isLp: Boolean): Order =
    if (isLp)
      fillMatchInfoInProofs(order, executedAmount, executedPrice)
    else
      order

  // end HACK for LP
}
