package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.{LevelDb, ReadWriteDb}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.meta.getSimpleName
import com.wavesplatform.dex.tool.OnComplete

trait ExchangeTxStorage[F[_]] {
  def put(tx: ExchangeTransaction): F[Unit]
  def transactionsByOrder(orderId: ByteStr): F[Seq[ExchangeTransaction]]
}

object ExchangeTxStorage {

  private val cls = getSimpleName(this)

  def levelDB[F[_]: OnComplete](db: LevelDb[F]): ExchangeTxStorage[F] = new ExchangeTxStorage[F] {

    override def put(tx: ExchangeTransaction): F[Unit] =
      measureDb(cls, "put") {
        db.readWrite { rw =>
          val txKey = DbKeys.exchangeTransaction(tx.id())
          if (!rw.has(txKey)) {
            rw.put(txKey, Some(tx))
            appendTxId(rw, tx.buyOrder.id(), tx.id())
            appendTxId(rw, tx.sellOrder.id(), tx.id())
          }
        }
      }

    override def transactionsByOrder(orderId: Order.Id): F[Seq[ExchangeTransaction]] =
      measureDb(cls, "transactionsByOrder") {
        db.readOnly { ro =>
          for {
            seqNr <- 1 to ro.get(DbKeys.orderTxIdsSeqNr(orderId))
            txId = ro.get(DbKeys.orderTxId(orderId, seqNr))
            tx <- ro.get(DbKeys.exchangeTransaction(txId))
          } yield tx
        }
      }

    private def appendTxId(rw: ReadWriteDb, orderId: ByteStr, txId: ByteStr): Unit = {
      val key = DbKeys.orderTxIdsSeqNr(orderId)
      val nextSeqNr = rw.get(key) + 1
      rw.put(key, nextSeqNr)
      rw.put(DbKeys.orderTxId(orderId, nextSeqNr), txId)
    }

  }

}
