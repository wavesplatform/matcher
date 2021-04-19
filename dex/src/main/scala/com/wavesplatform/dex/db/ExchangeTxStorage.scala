package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.{LevelDb, ReadWriteDb}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

trait ExchangeTxStorage[F[_]] {
  def put(tx: ExchangeTransaction): F[Unit]
}

object ExchangeTxStorage {

  def levelDB[F[_]](db: LevelDb[F]): ExchangeTxStorage[F] = new ExchangeTxStorage[F] {

    override def put(tx: ExchangeTransaction): F[Unit] = db.readWrite { rw =>
      val txKey = DbKeys.exchangeTransaction(tx.id())
      if (!rw.has(txKey)) {
        rw.put(txKey, Some(tx))
        appendTxId(rw, tx.buyOrder.id(), tx.id())
        appendTxId(rw, tx.sellOrder.id(), tx.id())
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
