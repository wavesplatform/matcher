package com.wavesplatform.dex.db

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}
import com.wavesplatform.dex.db.TestDbSync.sync

class EmptyOrderDb[F[_]: MonadError[*[_], Throwable]]() extends OrderDb[F] {

  override def containsInfo(id: Order.Id): F[Boolean] = sync(false.pure[F])
  override def status(id: Order.Id): F[OrderStatus.Final] = sync(OrderStatus.NotFound.pure[F].widen)
  override def get(id: Order.Id): F[Option[Order]] = sync(None.pure[F].widen)
  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): F[Unit] = sync(().pure[F])
  override def saveOrder(o: Order): F[Unit] = sync(().pure[F])

  override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): F[Seq[(Order.Id, OrderInfo[OrderStatus])]] =
    sync(Seq.empty.pure[F].widen)

  override def getOrderInfo(id: Id): F[Option[FinalOrderInfo]] = sync(None.pure[F].widen)
  override def transactionsByOrder(orderId: Id): F[Seq[ExchangeTransaction]] = sync(Seq.empty.pure[F].widen)

}
