package com.wavesplatform.dex.db

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.db.TestDbSync.sync
import com.wavesplatform.dex.domain.asset.AssetPair

class TestAssetPairDb[F[_]: MonadError[*[_], Throwable]] extends AssetPairsDb[F] {

  private var storage = Set[AssetPair]()
  override def add(pair: AssetPair): F[Unit] = sync((storage += pair).pure[F].void)
  override def remove(pair: AssetPair): F[Unit] = sync((storage -= pair).pure[F].void)
  override def all(): F[Set[AssetPair]] = sync(storage.pure[F])
}
