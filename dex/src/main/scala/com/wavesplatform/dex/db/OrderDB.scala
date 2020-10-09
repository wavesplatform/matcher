package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}
import org.iq80.leveldb.DB

/**
 * Contains only finalized orders
 */
trait OrderDB {
  def containsInfo(id: Order.Id): Boolean
  def status(id: Order.Id): OrderStatus.Final
  def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit
  def saveOrder(o: Order): Unit
  def get(id: Order.Id): Option[Order]
  def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): Seq[(Order.Id, OrderInfo[OrderStatus])]
  def getOrderInfo(id: Order.Id): Option[FinalOrderInfo]
  def transactionsByOrder(orderId: ByteStr): Seq[ExchangeTransaction]
}

object OrderDB {
  case class Settings(maxOrders: Int)

  def apply(settings: Settings, db: DB): OrderDB = new OrderDB with ScorexLogging {
    override def containsInfo(id: Order.Id): Boolean = db.readOnly(_.has(DbKeys.orderInfo(id)))

    override def status(id: Order.Id): OrderStatus.Final = db.readOnly { ro =>
      ro.get(DbKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)
    }

    override def saveOrder(o: Order): Unit = db.readWrite { rw =>
      val k = DbKeys.order(o.id())
      if (!rw.has(k))
        rw.put(k, Some(o))
    }

    override def get(id: Order.Id): Option[Order] = db.readOnly(_.get(DbKeys.order(id)))

    override def saveOrderInfo(id: Order.Id, sender: Address, oi: FinalOrderInfo): Unit = {
      val orderInfoKey = DbKeys.orderInfo(id)
      if (!db.has(orderInfoKey))
        db.readWrite { rw =>
          val newCommonSeqNr = rw.inc(DbKeys.finalizedCommonSeqNr(sender))
          rw.put(DbKeys.finalizedCommon(sender, newCommonSeqNr), Some(id))

          val newPairSeqNr = rw.inc(DbKeys.finalizedPairSeqNr(sender, oi.assetPair))
          rw.put(DbKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr), Some(id))
          if (newPairSeqNr > settings.maxOrders) // Indexes start with 1, so if maxOrders=100 and newPairSeqNr=101, we delete 1 (the first)
            rw.get(DbKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr - settings.maxOrders))
              .map(DbKeys.order)
              .foreach(x => rw.delete(x))

          rw.put(orderInfoKey, Some(oi))
        }
    }

    override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): Seq[(Order.Id, OrderInfo[OrderStatus])] =
      db.readOnly { ro =>
        val (seqNr, key) = maybePair match {
          case Some(p) =>
            (ro.get(DbKeys.finalizedPairSeqNr(owner, p)), DbKeys.finalizedPair(owner, p, _: Int))
          case None =>
            (ro.get(DbKeys.finalizedCommonSeqNr(owner)), DbKeys.finalizedCommon(owner, _: Int))
        }

        (for {
          offset <- 0 until math.min(seqNr, settings.maxOrders)
          id <- db.get(key(seqNr - offset))
          oi <- db.get(DbKeys.orderInfo(id))
        } yield id -> oi).sorted
      }

    override def getOrderInfo(id: Id): Option[FinalOrderInfo] = db.readOnly(_.get(DbKeys.orderInfo(id)))

    override def transactionsByOrder(orderId: Id): Seq[ExchangeTransaction] = db.readOnly { ro =>
      for {
        seqNr <- 1 to ro.get(DbKeys.orderTxIdsSeqNr(orderId))
        txId = ro.get(DbKeys.orderTxId(orderId, seqNr))
        tx <- ro.get(DbKeys.exchangeTransaction(txId))
      } yield tx
    }

  }

  implicit def orderInfoOrdering[S <: OrderStatus]: Ordering[(ByteStr, OrderInfo[S])] = Ordering.by { case (id, oi) => (-oi.timestamp, id) }
}
