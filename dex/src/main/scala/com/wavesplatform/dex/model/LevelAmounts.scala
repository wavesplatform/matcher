package com.wavesplatform.dex.model

import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group.catsSyntaxSemigroup
import cats.{Group, Monoid}
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.model.Events.OrderExecuted

case class LevelAmounts(asks: Map[Price, Amount] = Map.empty, bids: Map[Price, Amount] = Map.empty)
object LevelAmounts {
  val empty: LevelAmounts = LevelAmounts()

  def asks(xs: Map[Price, Amount]): LevelAmounts = new LevelAmounts(asks = xs)
  def bids(xs: Map[Price, Amount]): LevelAmounts = new LevelAmounts(bids = xs)
  def apply(tpe: OrderType, levelPrice: Price, levelAmount: Amount): LevelAmounts = {
    val xs = Map(levelPrice -> levelAmount)
    tpe.askBid(asks(xs), bids(xs))
  }

  trait LevelAmountsMonoid extends Monoid[LevelAmounts] {
    override val empty: LevelAmounts = LevelAmounts.empty
    override def combine(x: LevelAmounts, y: LevelAmounts): LevelAmounts = LevelAmounts(
      asks = x.asks |+| y.asks,
      bids = x.bids |+| y.bids
    )
  }

  implicit val levelAmountsMonoid: Monoid[LevelAmounts] = new LevelAmountsMonoid {}
  implicit val levelAmountsGroup: Group[LevelAmounts] = new Group[LevelAmounts] with LevelAmountsMonoid {
    override def inverse(x: LevelAmounts): LevelAmounts = LevelAmounts(asks = Group.inverse(x.asks), bids = Group.inverse(x.bids))
  }

  def mk(tpe: OrderType, levelPrice: Price, updatedSide: Side): LevelAmounts =
    LevelAmounts(tpe, levelPrice, updatedSide.get(levelPrice).fold(0L)(_.map(_.amount).sum))

  def mkDiff(levelPrice: Price, lo: LimitOrder): LevelAmounts       = LevelAmounts(lo.order.orderType, levelPrice, lo.amount)
  def mkDiff(levelPrice: Price, event: OrderExecuted): LevelAmounts = LevelAmounts(event.counter.order.orderType, levelPrice, event.executedAmount)
}
