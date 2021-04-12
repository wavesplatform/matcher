package com.wavesplatform.dex.db

import cats.MonadError
import cats.syntax.applicative._
import com.wavesplatform.dex.db.TestDbSync.sync
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

class TestOrderBookSnapshotDb[F[_]: MonadError[*[_], Throwable]] extends OrderBookSnapshotDb[F] {

  private var storage = Map[AssetPair, (Offset, OrderBookSnapshot)]()

  override def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]] =
    sync(storage.get(assetPair).pure[F])

  override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit] = sync {
    newSnapshot.foreach { s =>
      storage = storage.updated(assetPair, storage.getOrElse(assetPair, offset -> s))
    }.pure[F]
  }

  override def delete(assetPair: AssetPair): F[Unit] =
    sync((storage -= assetPair).pure[F])

}
