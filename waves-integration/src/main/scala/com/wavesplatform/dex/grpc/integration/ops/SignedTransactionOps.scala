package com.wavesplatform.dex.grpc.integration.ops

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.protobuf.transaction.SignedTransaction

object SignedTransactionOps {

  implicit final class Implicits(val self: SignedTransaction) extends AnyVal {

    def isExchangeTransaction: Boolean = self.transaction.exists(_.data.isExchange)

    def exchangeTransactionTraders: Set[Address] = self.transaction.flatMap(_.data.exchange).to(Set).flatMap { data =>
      data.orders.map(_.senderPublicKey.toVanillaPublicKey.toAddress)
    }

  }

}
