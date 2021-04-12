package com.wavesplatform.dex.db

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.db.TestDbSync.sync
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

class TestOrderDb[F[_]: MonadError[*[_], Throwable]](
  maxFinalizedOrders: Int
) extends OrderDb[F] {

  private var knownOrders = Map.empty[Order.Id, Order]
  private var orderInfo = Map.empty[Order.Id, OrderInfo[OrderStatus.Final]]
  private var idsForPair = Map.empty[(Address, AssetPair), Seq[Order.Id]].withDefaultValue(Seq.empty)
  private var idsForAddress = Map.empty[Address, Seq[Order.Id]].withDefaultValue(Seq.empty)
  private val txsByOrder = Map.empty[Order.Id, Seq[ExchangeTransaction]]

  override def containsInfo(id: Order.Id): F[Boolean] = sync {
    orderInfo.contains(id).pure[F]
  }

  override def get(id: Order.Id): F[Option[Order]] = sync {
    knownOrders.get(id).pure[F]
  }

  override def status(id: Order.Id): F[OrderStatus.Final] = sync {
    orderInfo.get(id).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status).pure[F]
  }

  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): F[Unit] =
    sync {
      containsInfo(id).map { contains =>
        if (!contains) {
          orderInfo += id -> oi
          idsForAddress += sender -> (id +: idsForAddress(sender)).take(maxFinalizedOrders)
          idsForPair += (sender, oi.assetPair) -> (id +: idsForPair(sender -> oi.assetPair)).take(maxFinalizedOrders)
        }
      }
    }

  override def saveOrder(o: Order): F[Unit] = sync {
    (knownOrders += o.id() -> o).pure[F]
  }

  override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]] =
    sync {
      (for {
        id <- maybePair.fold(idsForAddress(owner))(p => idsForPair(owner -> p))
        info <- orderInfo.get(id)
      } yield id -> info)
        .sortBy { case (_, oi) => -oi.timestamp }.pure[F].widen
    }

  override def getOrderInfo(id: Id): F[Option[FinalOrderInfo]] = sync {
    orderInfo.get(id).pure[F]
  }

  override def transactionsByOrder(orderId: Id): F[Seq[ExchangeTransaction]] = sync {
    txsByOrder.getOrElse(orderId, Seq.empty).pure[F]
  }

}
