package com.wavesplatform.dex.db

import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}
import scala.concurrent.{ExecutionContext, Future}

final class WaitingOrderDb private (implicit ex: ExecutionContext) extends OrderDb[Future] {

  private val ordersSaved = ConcurrentHashMap.newKeySet[Order.Id]()
  private val savedOrderInfo = ConcurrentHashMap.newKeySet[Order.Id]()

  val saveOrderInfoLatch = new CountDownLatch(1)
  val saveOrderLatch = new CountDownLatch(1)

  override def containsInfo(id: Order.Id): Future[Boolean] = Future {
    savedOrderInfo.contains(id)
  }

  override def status(id: Order.Id): Future[OrderStatus.Final] = Future.successful[OrderStatus.Final](OrderStatus.NotFound)
  override def get(id: Order.Id): Future[Option[Order]] = Future.successful(None)

  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Future[Unit] = Future {
    saveOrderInfoLatch.await()
    savedOrderInfo.add(id)
    ()
  }

  override def saveOrder(o: Order): Future[Unit] = Future {
    saveOrderLatch.await()
    ordersSaved.add(o.id())
    ()
  }

  override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): Future[Seq[(Order.Id, OrderInfo[OrderStatus])]] =
    Seq.empty.pure[Future].widen

  override def getOrderInfo(id: Id): Future[Option[FinalOrderInfo]] = None.pure[Future].widen
  override def transactionsByOrder(orderId: Id): Future[Seq[ExchangeTransaction]] = Seq.empty.pure[Future].widen

}

object WaitingOrderDb {

  def apply(implicit ec: ExecutionContext): WaitingOrderDb =
    new WaitingOrderDb

}
