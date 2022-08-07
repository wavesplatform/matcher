package com.wavesplatform.dex.domain.transaction

import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import monix.eval.Coeval

case class ExchangeTransactionV3(
  buyOrder: Order,
  sellOrder: Order,
  amount: Long,
  price: Long,
  buyMatcherFee: Long,
  sellMatcherFee: Long,
  fee: Long,
  timestamp: Long,
  proofs: Proofs
) extends ExchangeTransaction {
  override def version: Byte = 3
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves.getWavesTransaction))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves))
}

object ExchangeTransactionV3 {}

object M extends App {
//  import com.wavesplatform.dex.domain.order.OrderV3
//  OrderV3()
}
