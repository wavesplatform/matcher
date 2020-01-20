package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.leveldb.{DBExt, RW}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.Events._
import com.wavesplatform.dex.settings.MatcherSettings
import org.iq80.leveldb.DB

class MatcherTransactionWriter(db: DB) extends Actor with ScorexLogging {

  import MatcherTransactionWriter._

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      saveExchangeTx(tx)
  }

  private def saveExchangeTx(tx: ExchangeTransaction): Unit = db.readWrite { rw =>
    log.trace(s"Appending ${tx.id()} to orders [${tx.buyOrder.idStr()}, ${tx.sellOrder.idStr()}]")
    rw.put(MatcherKeys.exchangeTransaction(tx.id()), Some(tx))
    appendTxId(rw, tx.buyOrder.id(), tx.id())
    appendTxId(rw, tx.sellOrder.id(), tx.id())
  }
}

object MatcherTransactionWriter {

  def name: String = "MatcherTransactionWriter"

  def props(db: DB, settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(db))

  private def appendTxId(rw: RW, orderId: ByteStr, txId: ByteStr): Unit = {
    val key       = MatcherKeys.orderTxIdsSeqNr(orderId)
    val nextSeqNr = rw.get(key) + 1
    rw.put(key, nextSeqNr)
    rw.put(MatcherKeys.orderTxId(orderId, nextSeqNr), txId)
  }
}
