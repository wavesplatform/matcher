package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.instances.future._
import com.wavesplatform.dex.domain.asset.AssetPair

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.IteratorHasAsScala

class TestAssetPairDb[F[_]: Applicative] private () extends AssetPairsDb[F] {

  private val storage = ConcurrentHashMap.newKeySet[AssetPair]()

  override def add(pair: AssetPair): F[Unit] = storage.add(pair).pure[F].void
  override def remove(pair: AssetPair): F[Unit] = storage.remove(pair).pure[F].void
  override def all(): F[Set[AssetPair]] = storage.iterator().asScala.toSet.pure[F]
}

object TestAssetPairDb {

  def apply(): TestAssetPairDb[Future] = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((command: Runnable) => command.run())
    new TestAssetPairDb()
  }

}
