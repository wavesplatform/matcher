package com.wavesplatform.dex.model

import cats.Group
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group._
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.model.LevelAmounts.mkDiff

case class LevelAmounts(asks: Map[Price, Amount] = Map.empty, bids: Map[Price, Amount] = Map.empty) {
  def put(other: LevelAmounts): LevelAmounts                          = LevelAmounts(asks ++ other.asks, bids ++ other.bids)
  def add(levelPrice: Price, lo: LimitOrder): LevelAmounts            = this |+| mkDiff(levelPrice, lo)
  def subtract(levelPrice: Price, event: OrderExecuted): LevelAmounts = this |-| mkDiff(levelPrice, event)
}

object LevelAmounts {
  val empty: LevelAmounts = LevelAmounts()

  def apply(tpe: OrderType, levelPrice: Price, side: Side): LevelAmounts =
    LevelAmounts(tpe, levelPrice, side.get(levelPrice).fold(0L)(_.map(_.amount).sum))

  def asks(xs: Map[Price, Amount]): LevelAmounts = new LevelAmounts(asks = xs)
  def bids(xs: Map[Price, Amount]): LevelAmounts = new LevelAmounts(bids = xs)
  def apply(tpe: OrderType, levelPrice: Price, levelAmount: Amount): LevelAmounts = {
    val xs = Map(levelPrice -> levelAmount)
    tpe.askBid(asks(xs), bids(xs))
  }

  private implicit val levelAmountsGroup: Group[LevelAmounts] = new Group[LevelAmounts] {
    override val empty: LevelAmounts = LevelAmounts.empty
    override def combine(x: LevelAmounts, y: LevelAmounts): LevelAmounts = LevelAmounts(
      asks = x.asks |+| y.asks,
      bids = x.bids |+| y.bids
    )
    override def inverse(x: LevelAmounts): LevelAmounts = LevelAmounts(asks = Group.inverse(x.asks), bids = Group.inverse(x.bids))
  }

  private def mkDiff(levelPrice: Price, lo: LimitOrder): LevelAmounts = LevelAmounts(lo.order.orderType, levelPrice, lo.amount)
  private def mkDiff(levelPrice: Price, event: OrderExecuted): LevelAmounts =
    LevelAmounts(event.counter.order.orderType, levelPrice, event.executedAmount)
}
