package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.instances.future._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

import scala.concurrent.{ExecutionContext, Future}

class TestOrderDb[F[_]: Applicative] private (maxFinalizedOrders: Int) extends OrderDb[F] {

  private var knownOrders = Map.empty[Order.Id, Order]
  private var orderInfo = Map.empty[Order.Id, OrderInfo[OrderStatus.Final]]
  private var idsForPair = Map.empty[(Address, AssetPair), Seq[Order.Id]].withDefaultValue(Seq.empty)
  private var idsForAddress = Map.empty[Address, Seq[Order.Id]].withDefaultValue(Seq.empty)
  private val txsByOrder = Map.empty[Order.Id, Seq[ExchangeTransaction]]

  override def containsInfo(id: Order.Id): F[Boolean] =
    synchronized(orderInfo.contains(id)).pure[F]

  override def get(id: Order.Id): F[Option[Order]] =
    synchronized(knownOrders.get(id)).pure[F]

  override def status(id: Order.Id): F[OrderStatus.Final] =
    synchronized(orderInfo.get(id).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)).pure[F]

  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): F[Unit] =
    synchronized {
      if (!orderInfo.contains(id)) {
        orderInfo += id -> oi
        idsForAddress += sender -> (id +: idsForAddress(sender)).take(maxFinalizedOrders)
        idsForPair += (sender, oi.assetPair) -> (id +: idsForPair(sender -> oi.assetPair)).take(maxFinalizedOrders)
      }
    }
      .pure[F]

  override def saveOrder(o: Order): F[Unit] =
    synchronized(knownOrders += o.id() -> o).pure[F]

  override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]] =
    synchronized {
      (for {
        id <- maybePair.fold(idsForAddress(owner))(p => idsForPair(owner -> p))
        info <- orderInfo.get(id)
      } yield id -> info).sortBy { case (_, oi) => -oi.timestamp }
    }.pure[F].widen

  override def getOrderInfo(id: Id): F[Option[FinalOrderInfo]] =
    synchronized(orderInfo.get(id)).pure[F]

  override def transactionsByOrder(orderId: Id): F[Seq[ExchangeTransaction]] =
    synchronized(txsByOrder.getOrElse(orderId, Seq.empty)).pure[F]

}

object TestOrderDb {

  def apply(maxFinalizedOrders: Int): TestOrderDb[Future] = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((command: Runnable) => command.run())
    new TestOrderDb[Future](maxFinalizedOrders)
  }

}
