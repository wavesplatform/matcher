package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.instances.future._
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}

class TestOrderBookSnapshotDb[F[_]: Applicative] private () extends OrderBookSnapshotDb[F] {

  private val storage = new ConcurrentHashMap[AssetPair, (Offset, OrderBookSnapshot)]()

  override def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]] =
    Option(storage.get(assetPair)).pure[F]

  override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit] =
    storage.compute(assetPair, (_, current) => (offset, newSnapshot.getOrElse(current._2))).pure[F].void

  override def delete(assetPair: AssetPair): F[Unit] =
    storage.remove(assetPair).pure[F].void

}

object TestOrderBookSnapshotDb {

  def apply(): TestOrderBookSnapshotDb[Future] = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((command: Runnable) => command.run())
    new TestOrderBookSnapshotDb[Future]()
  }

}
