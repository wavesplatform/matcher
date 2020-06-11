package com.wavesplatform.dex.api

import com.wavesplatform.dex.DbKeys
import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import org.iq80.leveldb.DB

object DBUtils {

  def order(db: DB, orderId: ByteStr): Option[Order]              = db.get(DbKeys.order(orderId))
  def orderInfo(db: DB, orderId: ByteStr): Option[FinalOrderInfo] = db.get(DbKeys.orderInfo(orderId))

  def transactionsForOrder(db: DB, orderId: ByteStr): Seq[ExchangeTransaction] = db.readOnly { ro =>
    for {
      seqNr <- 1 to ro.get(DbKeys.orderTxIdsSeqNr(orderId))
      txId = ro.get(DbKeys.orderTxId(orderId, seqNr))
      tx <- ro.get(DbKeys.exchangeTransaction(txId))
    } yield tx
  }
}
