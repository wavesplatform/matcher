package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.instances.future._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

class TestRateDb[F[_]: Applicative] private () extends RateDb[F] {

  private val rates = TrieMap.empty[IssuedAsset, Double]

  override def upsertRate(asset: Asset.IssuedAsset, value: Double): F[Unit] = (rates += asset -> value).pure[F].void

  override def getAllRates: F[Map[Asset.IssuedAsset, Double]] = rates.toMap.pure[F]

  override def deleteRate(asset: Asset.IssuedAsset): F[Unit] = (rates -= asset).pure[F].void

}

object TestRateDb {

  def apply(): TestRateDb[Future] = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor((command: Runnable) => command.run())
    new TestRateDb[Future]()
  }

}
