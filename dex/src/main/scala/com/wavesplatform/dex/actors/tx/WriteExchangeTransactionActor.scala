package com.wavesplatform.dex.actors.tx

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.db.DbKeys
import com.wavesplatform.dex.db.leveldb.{DBExt, RW}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.Events._
import org.iq80.leveldb.DB

class WriteExchangeTransactionActor(db: DB) extends Actor with ScorexLogging {

  import WriteExchangeTransactionActor._

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) => saveExchangeTx(tx)
  }

  private def saveExchangeTx(tx: ExchangeTransaction): Unit = db.readWrite { rw =>
    log.trace(s"Appending ${tx.id()} to orders [${tx.buyOrder.idStr()}, ${tx.sellOrder.idStr()}]")
    val txKey = DbKeys.exchangeTransaction(tx.id())
    if (!rw.has(txKey)) {
      rw.put(txKey, Some(tx))
      appendTxId(rw, tx.buyOrder.id(), tx.id())
      appendTxId(rw, tx.sellOrder.id(), tx.id())
    }
  }

}

object WriteExchangeTransactionActor {

  def name: String = "WriteExchangeTransactionActor"

  def props(db: DB): Props = Props(new WriteExchangeTransactionActor(db))

  def appendTxId(rw: RW, orderId: ByteStr, txId: ByteStr): Unit = {
    val key = DbKeys.orderTxIdsSeqNr(orderId)
    val nextSeqNr = rw.get(key) + 1
    rw.put(key, nextSeqNr)
    rw.put(DbKeys.orderTxId(orderId, nextSeqNr), txId)
  }

}
