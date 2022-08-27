package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}
import org.iq80.leveldb.DB

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

/**
 * Contains only finalized orders
 */
trait OrderDb[F[_]] {
  def containsInfo(id: Order.Id): F[Boolean]
  def status(id: Order.Id): F[OrderStatus.Final]
  def saveOrderInfo(id: Order.Id, oi: OrderInfo[OrderStatus.Final]): F[Unit]
  def saveOrderInfoForHistory(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): F[Unit]
  def saveOrder(o: Order): F[Unit]
  def get(id: Order.Id): F[Option[Order]]
  def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]]
  def getOrderInfo(id: Order.Id): F[Option[FinalOrderInfo]]
}

object OrderDb {
  case class Settings(maxOrders: Int)

  def levelDb(settings: Settings, db: DB): OrderDb[Future] =
    levelDb(settings, db, Map(0 -> ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))))

  def levelDb(settings: Settings, db: DB, levelDbEcMap: Map[Int, ExecutionContextExecutorService]): OrderDb[Future] = new OrderDb[Future] {

    override def containsInfo(id: Order.Id): Future[Boolean] = Future {
      db.readOnly(_.has(DbKeys.orderInfo(id)))
    }(getEcByOrderId(id))

    override def status(id: Order.Id): Future[OrderStatus.Final] = Future {
      db.readOnly(ro => ro.get(DbKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status))
    }(getEcByOrderId(id))

    override def saveOrder(o: Order): Future[Unit] = Future {
      db.readWrite { rw =>
        val k = DbKeys.order(o.id())
        if (!rw.has(k))
          rw.put(k, Some(o))
      }
    }(getEcByOrderId(o.id()))

    override def get(id: Order.Id): Future[Option[Order]] = Future {
      db.readOnly(_.get(DbKeys.order(id)))
    }(getEcByOrderId(id))

    override def saveOrderInfo(id: Order.Id, oi: FinalOrderInfo): Future[Unit] = Future {
      val orderInfoKey = DbKeys.orderInfo(id)
      db.readWrite { rw =>
        if (!rw.has(orderInfoKey))
          rw.put(orderInfoKey, Some(oi))
      }
    }(getEcByOrderId(id))

    override def saveOrderInfoForHistory(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Future[Unit] = Future {
      val orderInfoKey = DbKeys.orderInfoForHistory(id)
      db.readWrite { rw =>
        if (!rw.has(orderInfoKey)) {
          val newCommonSeqNr = rw.inc(DbKeys.finalizedCommonSeqNr(sender))
          rw.put(DbKeys.finalizedCommon(sender, newCommonSeqNr), Some(id))

          val newPairSeqNr = rw.inc(DbKeys.finalizedPairSeqNr(sender, oi.assetPair))
          rw.put(DbKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr), Some(id))
          if (newPairSeqNr > settings.maxOrders) // Indexes start with 1, so if maxOrders=100 and newPairSeqNr=101, we delete 1 (the first)
            rw.get(DbKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr - settings.maxOrders))
              .map(DbKeys.order)
              .foreach(x => rw.delete(x))
        }
      }
    }(getEcBySender(sender))

    override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): Future[Seq[(Order.Id, OrderInfo[OrderStatus])]] = {
      val ec = getEcBySender(owner)
      Future {
        db.readOnly { ro =>
          val (seqNr, key) =
            maybePair match {
              case Some(p) =>
                (ro.get(DbKeys.finalizedPairSeqNr(owner, p)), DbKeys.finalizedPair(owner, p, _: Int))
              case None =>
                (ro.get(DbKeys.finalizedCommonSeqNr(owner)), DbKeys.finalizedCommon(owner, _: Int))
            }

          for {
            offset <- 0 until math.min(seqNr, settings.maxOrders)
            id <- ro.get(key(seqNr - offset))
            oi <- ro.get(DbKeys.orderInfo(id))
          } yield id -> oi
        }
      }(ec).map(_.sorted(orderInfoOrdering))(ec)
    }

    override def getOrderInfo(id: Order.Id): Future[Option[FinalOrderInfo]] = Future {
      db.readOnly(_.get(DbKeys.orderInfo(id)))
    }(getEcByOrderId(id))

    //0x7FFFFFFF "hack" is taken from java.util.Hashtable in order to deal with negative hash codes

    private def getEcByOrderId(id: Order.Id): ExecutionContextExecutorService =
      levelDbEcMap((id.base58.hashCode & 0x7fffffff) % levelDbEcMap.size)

    private def getEcBySender(sender: Address): ExecutionContextExecutorService =
      levelDbEcMap((sender.stringRepr.hashCode & 0x7fffffff) % levelDbEcMap.size)

  }

  val orderIdOrdering: Ordering[(Order.Id, Long)] = Ordering.by { case (id, ts) => (-ts, id) }

  def orderInfoOrdering: Ordering[(ByteStr, OrderInfo[OrderStatus])] = orderIdOrdering.on {
    case (id, orderInfo) => (id, orderInfo.timestamp)
  }

}
