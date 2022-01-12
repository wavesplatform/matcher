package com.wavesplatform.dex.domain.state

import cats.kernel.instances.map._
import cats.{Monoid, Semigroup}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}

import scala.collection.immutable.Map

case class Portfolio(balance: Long, lease: LeaseBalance, assets: Map[IssuedAsset, Long]) {

  lazy val spendableBalance: Long = balance - lease.out

  def balanceOf(assetId: Asset): Long = assetId match {
    case Waves => balance
    case asset @ IssuedAsset(_) => assets.getOrElse(asset, 0L)
  }

}

object Portfolio {

  val empty: Portfolio = Portfolio(0L, Monoid[LeaseBalance].empty, Map.empty)

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val monoid: Monoid[Portfolio] = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio.empty

    override def combine(older: Portfolio, newer: Portfolio): Portfolio = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      lease = Monoid.combine(older.lease, newer.lease),
      assets = Monoid.combine(older.assets, newer.assets)
    )

  }

  implicit class PortfolioExt(self: Portfolio) {

    def spendableBalanceOf(assetId: Asset): Long = assetId.fold(self.spendableBalance)(self.assets.getOrElse(_, 0L))

    def assetIds: Set[Asset] = self.assets.keySet ++ Set(Waves)

    def changedAssetIds(that: Portfolio): Set[Asset] = {
      val a1 = assetIds
      val a2 = that.assetIds

      val intersection = a1 & a2
      val sureChanged = (a1 | a2) -- intersection

      intersection.filter(x => spendableBalanceOf(x) != that.spendableBalanceOf(x)) ++ sureChanged
    }

  }

}
