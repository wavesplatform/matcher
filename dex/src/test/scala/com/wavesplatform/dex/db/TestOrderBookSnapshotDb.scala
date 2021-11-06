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

import scala.jdk.CollectionConverters._

class TestOrderBookSnapshotDb[F[_]: Applicative] private () extends OrderBookSnapshotDb[F] {

  private val storage = new ConcurrentHashMap[AssetPair, (Offset, OrderBookSnapshot)]()

  override def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]] =
    Option(storage.get(assetPair)).pure[F]

  override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit] =
    storage.compute(assetPair, (_, current) => (offset, newSnapshot.getOrElse(current._2))).pure[F].void

  override def delete(assetPair: AssetPair): F[Unit] =
    storage.remove(assetPair).pure[F].void

  override def iterateOffsets(pred: AssetPair => Boolean): F[Map[AssetPair, Offset]] =
    storage.asScala.toMap.view.filterKeys(pred).mapValues(_._1).toMap.pure[F]

  override def iterateSnapshots(pred: AssetPair => Boolean): F[Map[AssetPair, OrderBookSnapshot]] =
    storage.asScala.toMap.view.filterKeys(pred).mapValues(_._2).toMap.pure[F]

}

object TestOrderBookSnapshotDb {

  def apply(): TestOrderBookSnapshotDb[Future] = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((command: Runnable) => command.run())
    new TestOrderBookSnapshotDb[Future]()
  }

}
