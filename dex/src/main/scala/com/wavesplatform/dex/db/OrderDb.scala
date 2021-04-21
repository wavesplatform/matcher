package com.wavesplatform.dex.db

import cats.Functor
import cats.syntax.functor._
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

/**
 * Contains only finalized orders
 */
trait OrderDb[F[_]] {
  def containsInfo(id: Order.Id): F[Boolean]
  def status(id: Order.Id): F[OrderStatus.Final]
  def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): F[Unit]
  def saveOrder(o: Order): F[Unit]
  def get(id: Order.Id): F[Option[Order]]
  def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]]
  def getOrderInfo(id: Order.Id): F[Option[FinalOrderInfo]]
  def transactionsByOrder(orderId: ByteStr): F[Seq[ExchangeTransaction]]
}

object OrderDb {
  case class Settings(maxOrders: Int)

  def levelDb[F[_]: Functor](settings: Settings, levelDb: LevelDb[F]): OrderDb[F] = new OrderDb[F] {

    override def containsInfo(id: Order.Id): F[Boolean] =
      levelDb.readOnly(_.has(DbKeys.orderInfo(id)))

    override def status(id: Order.Id): F[OrderStatus.Final] = levelDb.readOnly { ro =>
      ro.get(DbKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)
    }

    override def saveOrder(o: Order): F[Unit] = levelDb.readWrite { rw =>
      val k = DbKeys.order(o.id())
      if (!rw.has(k))
        rw.put(k, Some(o))
    }

    override def get(id: Order.Id): F[Option[Order]] = levelDb.readOnly(_.get(DbKeys.order(id)))

    override def saveOrderInfo(id: Order.Id, sender: Address, oi: FinalOrderInfo): F[Unit] = {
      val orderInfoKey = DbKeys.orderInfo(id)

      levelDb.readWrite { rw =>
        if (!rw.has(orderInfoKey)) {
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
    }

    override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]] =
      levelDb.readOnly { ro =>
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
      }.map(_.sorted(orderInfoOrdering))

    override def getOrderInfo(id: Id): F[Option[FinalOrderInfo]] =
      levelDb.readOnly(_.get(DbKeys.orderInfo(id)))

    override def transactionsByOrder(orderId: Id): F[Seq[ExchangeTransaction]] = levelDb.readOnly { ro =>
      for {
        seqNr <- 1 to ro.get(DbKeys.orderTxIdsSeqNr(orderId))
        txId = ro.get(DbKeys.orderTxId(orderId, seqNr))
        tx <- ro.get(DbKeys.exchangeTransaction(txId))
      } yield tx
    }

  }

  val orderIdOrdering: Ordering[(Order.Id, Long)] = Ordering.by { case (id, ts) => (-ts, id) }

  def orderInfoOrdering: Ordering[(ByteStr, OrderInfo[OrderStatus])] = orderIdOrdering.on {
    case (id, orderInfo) => (id, orderInfo.timestamp)
  }

}
