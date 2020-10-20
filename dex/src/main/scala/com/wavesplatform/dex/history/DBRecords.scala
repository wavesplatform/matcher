package com.wavesplatform.dex.history

import java.time.LocalDateTime

import com.wavesplatform.dex.model.Events.EventReason

object DBRecords {

  sealed trait Record

  case class OrderRecord(
    id: String,
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
    created: LocalDateTime,
    closedAt: Option[LocalDateTime]
  ) extends Record

  case class EventRecord(
    orderId: String,
    eventType: Byte,
    timestamp: LocalDateTime,
    price: BigDecimal,
    filled: BigDecimal,
    totalFilled: BigDecimal,
    feeFilled: BigDecimal,
    feeTotalFilled: BigDecimal,
    status: Byte,
    reason: EventReason
  ) extends Record

}
