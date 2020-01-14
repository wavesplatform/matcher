package com.wavesplatform.dex.db

import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}
import com.wavesplatform.dex.settings.MatcherSettings
import org.iq80.leveldb.DB

trait OrderDB {
  def containsInfo(id: Order.Id): Boolean
  def status(id: Order.Id): OrderStatus.Final
  def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit
  def saveOrder(o: Order): Unit
  def get(id: Order.Id): Option[Order]
  def loadRemainingOrders(owner: Address,
                          maybePair: Option[AssetPair],
                          activeOrders: Seq[(Order.Id, OrderInfo[OrderStatus])]): Seq[(Order.Id, OrderInfo[OrderStatus])]
}

object OrderDB {
  val OldestOrderIndexOffset = 100

  def apply(settings: MatcherSettings, db: DB): OrderDB = new OrderDB with ScorexLogging {
    override def containsInfo(id: Order.Id): Boolean = db.readOnly(_.has(MatcherKeys.orderInfo(id)))

    override def status(id: Order.Id): OrderStatus.Final = db.readOnly { ro =>
      ro.get(MatcherKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)
    }

    override def saveOrder(o: Order): Unit = db.readWrite { rw =>
      val k = MatcherKeys.order(o.id())
      if (!rw.has(k)) {
        rw.put(k, Some(o))
      }
    }

    override def get(id: Order.Id): Option[Order] = db.readOnly(_.get(MatcherKeys.order(id)))

    override def saveOrderInfo(id: Order.Id, sender: Address, oi: FinalOrderInfo): Unit = {
      val orderInfoKey = MatcherKeys.orderInfo(id)
      if (db.has(orderInfoKey)) log.warn(s"Finalized order info already exists for $id")
      else {
        db.readWrite { rw =>
          val newCommonSeqNr = rw.inc(MatcherKeys.finalizedCommonSeqNr(sender))
          rw.put(MatcherKeys.finalizedCommon(sender, newCommonSeqNr), Some(id))

          val newPairSeqNr = rw.inc(MatcherKeys.finalizedPairSeqNr(sender, oi.assetPair))
          rw.put(MatcherKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr), Some(id))
          if (newPairSeqNr > OldestOrderIndexOffset) // Indexes start with 1, so if newPairSeqNr=101, we delete 1 (the first)
            rw.get(MatcherKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr - OldestOrderIndexOffset))
              .map(MatcherKeys.order)
              .foreach(x => rw.delete(x))

          rw.put(orderInfoKey, Some(oi))
        }
      }
    }

    override def loadRemainingOrders(owner: Address,
                                     maybePair: Option[AssetPair],
                                     activeOrders: Seq[(Order.Id, OrderInfo[OrderStatus])]): Seq[(Order.Id, OrderInfo[OrderStatus])] = db.readOnly {
      ro =>
        val (seqNr, key) = maybePair match {
          case Some(p) =>
            (ro.get(MatcherKeys.finalizedPairSeqNr(owner, p)), MatcherKeys.finalizedPair(owner, p, _: Int))
          case None =>
            (ro.get(MatcherKeys.finalizedCommonSeqNr(owner)), MatcherKeys.finalizedCommon(owner, _: Int))
        }

        activeOrders ++ (for {
          offset <- 0 until (settings.maxOrdersPerRequest - activeOrders.length)
          id     <- db.get(key(seqNr - offset))
          oi     <- db.get(MatcherKeys.orderInfo(id))
        } yield id -> oi).sorted
    }
  }

  implicit def orderInfoOrdering[S <: OrderStatus]: Ordering[(ByteStr, OrderInfo[S])] = Ordering.by { case (id, oi) => (-oi.timestamp, id) }
}
