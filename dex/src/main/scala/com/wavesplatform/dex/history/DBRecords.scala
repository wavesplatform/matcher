package com.wavesplatform.dex.history

import java.time.LocalDateTime

object DBRecords {

  sealed trait Record

  case class OrderRecord(id: String,
                         tpe: Byte,
                         senderAddress: String,
                         senderPublicKey: String,
                         amountAssetId: String,
                         priceAssetId: String,
                         feeAssetId: String,
                         side: Byte,
                         price: BigDecimal,
                         amount: BigDecimal,
                         timestamp: LocalDateTime,
                         expiration: LocalDateTime,
                         fee: BigDecimal,
                         created: LocalDateTime)
      extends Record

  case class EventRecord(orderId: String,
                         eventType: Byte,
                         timestamp: LocalDateTime,
                         price: BigDecimal,
                         filled: BigDecimal,
                         totalFilled: BigDecimal,
                         feeFilled: BigDecimal,
                         feeTotalFilled: BigDecimal,
                         status: Byte)
      extends Record
}
