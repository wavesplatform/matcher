package com.wavesplatform.dex.model

case class OrderBookAggregatedSnapshot(bids: Seq[LevelAgg] = Seq.empty, asks: Seq[LevelAgg] = Seq.empty) {
  def getSideFor(acceptedOrder: AcceptedOrder): Seq[LevelAgg] = if (acceptedOrder.isBuyOrder) bids else asks
  def getCounterSideFor(acceptedOrder: AcceptedOrder): Seq[LevelAgg] = if (acceptedOrder.isBuyOrder) asks else bids
}

object OrderBookAggregatedSnapshot {
  val empty: OrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot()
}
