package com.wavesplatform.dex.model

import cats.instances.list.catsStdInstancesForList
import cats.kernel.Group
import cats.syntax.foldable._
import cats.syntax.group._
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events._
import com.wavesplatform.dex.settings.MatchingRule

import scala.collection.immutable.{HashMap, Queue, TreeMap}

case class OrderBook private (bids: Side, asks: Side, lastTrade: Option[LastTrade], orderIds: HashMap[Order.Id, (OrderType, Price)]) {
  import OrderBook._

  def bestBid: Option[LevelAgg] = bids.bestLevel
  def bestAsk: Option[LevelAgg] = asks.bestLevel

  def allOrders: Iterator[LimitOrder] = (bids.valuesIterator ++ asks.valuesIterator).flatten

  def cancel(orderId: ByteStr, reason: OrderCanceledReason, timestamp: Long): (OrderBook, Option[Event], LevelAmounts) = {
    def mkEvent(lo: LimitOrder): Option[OrderCanceled] = Some(OrderCanceled(lo, reason, timestamp))

    orderIds.get(orderId).fold((this, Option.empty[OrderCanceled], LevelAmounts.empty)) {
      case (orderType, price) =>
        val (updatedAsks, updatedBids, lo) = if (orderType == OrderType.SELL) {
          val (updatedAsks, lo) = asks.unsafeRemove(price, orderId)
          (updatedAsks, bids, lo)
        } else {
          val (updatedBids, lo) = bids.unsafeRemove(price, orderId)
          (asks, updatedBids, lo)
        }

        val updatedOrderIds = orderIds - orderId

        (
          copy(asks = updatedAsks, bids = updatedBids, orderIds = updatedOrderIds),
          mkEvent(lo),
          Group.inverse(LevelAmounts.mkDiff(price, lo))
        )
    }
  }

  def cancelAll(ts: Long, reason: OrderCanceledReason): OrderBookUpdates = {
    val orders         = allOrders.toList
    val canceledOrders = Queue(orders.map { OrderCanceled(_, reason, ts) }: _*)
    val levelAmounts = orders.foldMap { o =>
      // Order MUST be in orderIds. It's okay to fail here with Map.apply if the implementation is wrong
      LevelAmounts.mkDiff(orderIds.getOrElse(o.id, throw new IllegalStateException(s"Order ids doesn't contain the order ${o.id}"))._2, o)
    }

    OrderBookUpdates(OrderBook.empty, canceledOrders, Group.inverse(levelAmounts), None)
  }

  def add(submitted: AcceptedOrder,
          eventTs: Long,
          getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long),
          tickSize: Long = MatchingRule.DefaultRule.tickSize): OrderBookUpdates = {
    val events = Queue(OrderAdded(submitted, OrderAddedReason.RequestExecuted, eventTs))
    if (submitted.order.isValid(eventTs)) doMatch(eventTs, tickSize, getMakerTakerFee, submitted, events, this)
    else OrderBookUpdates(this, events.enqueue(OrderCanceled(submitted, OrderCanceledReason.BecameInvalid, eventTs)), LevelAmounts.empty, None)
  }

  def snapshot: OrderBookSnapshot                     = OrderBookSnapshot(bids, asks, lastTrade)
  def aggregatedSnapshot: OrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString: String = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""

  def side(tpe: OrderType): Side = tpe.askBid(asks, bids)

  def best(tpe: OrderType): Option[(Price, LimitOrder)] = side(tpe).best

  private def unsafeWithoutBest(tpe: OrderType): OrderBook = tpe.askBid(
    {
      val (updatedSide, removedOrderId) = asks.unsafeWithoutBest
      copy(asks = updatedSide, orderIds = orderIds - removedOrderId)
    }, {
      val (updatedSide, removedOrderId) = bids.unsafeWithoutBest
      copy(bids = updatedSide, orderIds = orderIds - removedOrderId)
    }
  )

  /**
    * Note: orderIds aren't changed, because updated has the same inner order
    */
  private def unsafeUpdateBest(updated: LimitOrder): OrderBook = updated.order.orderType.askBid(
    copy(asks = asks.unsafeUpdateBest(updated)),
    copy(bids = bids.unsafeUpdateBest(updated))
  )

  def insert(levelPrice: Price, lo: LimitOrder): OrderBook = {
    val updatedOrderIds = orderIds.updated(lo.order.id(), (lo.order.orderType, levelPrice))
    lo.order.orderType.askBid(
      copy(asks = asks.put(levelPrice, lo), orderIds = updatedOrderIds),
      copy(bids = bids.put(levelPrice, lo), orderIds = updatedOrderIds)
    )
  }
}

object OrderBook {

  /**
    * Corrects order price by the tick size in favor of the client.
    * Buy order prices are rounded '''down''', sell order prices are rounded '''upwards'''
    */
  def correctPriceByTickSize(price: Price, orderType: OrderType, normalizedTickSize: Long): Price =
    if (price % normalizedTickSize == 0) price
    else
      orderType match {
        case OrderType.BUY  => price / normalizedTickSize * normalizedTickSize
        case OrderType.SELL => (price / normalizedTickSize + 1) * normalizedTickSize
      }

  /**
    * @param submitted It is expected, that submitted is valid on eventTs and it is a new order
    */
  private def doMatch(eventTs: Long,
                      tickSize: Long,
                      getMakerTakerMaxFee: (AcceptedOrder, LimitOrder) => (Long, Long),
                      submitted: AcceptedOrder,
                      events: Queue[Event],
                      orderBook: OrderBook): OrderBookUpdates = {

    def unmatchable(ao: AcceptedOrder): OrderCanceled = OrderCanceled(ao, OrderCanceledReason.BecameUnmatchable, eventTs)

    @scala.annotation.tailrec
    def loop(submitted: AcceptedOrder, currentUpdates: OrderBookUpdates): OrderBookUpdates =
      currentUpdates.orderBook.best(submitted.order.orderType.opposite) match {
        case Some((levelPrice, counter)) if overlaps(submitted, levelPrice) =>
          if (!submitted.isValid(counter.price)) currentUpdates.copy(events = currentUpdates.events enqueue unmatchable(submitted))
          else if (counter.order.isValid(eventTs)) {

            val (counterExecutedFee, submittedExecutedFee) = getMakerTakerMaxFee(submitted, counter)
            val orderExecutedEvent                         = OrderExecuted(submitted, counter, eventTs, counterExecutedFee, submittedExecutedFee)

            if (orderExecutedEvent.executedAmount == 0) currentUpdates.copy(events = currentUpdates.events enqueue unmatchable(submitted))
            else {

              val updatedEvents = currentUpdates.events.enqueue(orderExecutedEvent)
              val lastTrade     = Some(LastTrade(counter.price, orderExecutedEvent.executedAmount, submitted.order.orderType))

              val submittedRemaining = orderExecutedEvent.submittedRemaining
              val counterRemaining   = orderExecutedEvent.counterRemaining

              val (updatedOrderBook, updatedLevelChanges) = {
                val updatedLevelChanges = currentUpdates.levelChanges.subtract(levelPrice, orderExecutedEvent)
                val ob                  = currentUpdates.orderBook.copy(lastTrade = lastTrade)

                if (counterRemaining.isValid) (ob.unsafeUpdateBest(counterRemaining), updatedLevelChanges)
                else
                  (
                    ob.unsafeWithoutBest(counter.order.orderType),
                    updatedLevelChanges |-| LevelAmounts.mkDiff(levelPrice, counterRemaining)
                  )
              }

              val newUpdates = currentUpdates.copy(updatedOrderBook, updatedEvents, updatedLevelChanges, lastTrade)

              if (submittedRemaining.isValid) {
                if (counterRemaining.isValid)
                  // if submitted is not filled (e.g. LimitOrder: rounding issues, MarkerOrder: afs = 0) cancel its remaining
                  newUpdates.copy(events = updatedEvents enqueue unmatchable(submittedRemaining))
                else {
                  submittedRemaining match {
                    case submittedRemaining: LimitOrder => loop(submittedRemaining, newUpdates)
                    case submittedRemaining: MarketOrder =>
                      val canSpendMore = submittedRemaining.availableForSpending > 0
                      if (canSpendMore) loop(submittedRemaining, newUpdates)
                      else newUpdates.copy(events = updatedEvents enqueue unmatchable(submittedRemaining))
                  }
                }
              } else newUpdates
            }
          } else
            loop(
              submitted = submitted,
              currentUpdates = currentUpdates
                .copy(
                  orderBook = currentUpdates.orderBook.unsafeWithoutBest(counter.order.orderType),
                  events = currentUpdates.events.enqueue(OrderCanceled(counter, OrderCanceledReason.BecameInvalid, eventTs)),
                  levelChanges = currentUpdates.levelChanges |-| LevelAmounts.mkDiff(levelPrice, counter)
                )
            )

        case _ =>
          submitted match {
            case submitted: LimitOrder =>
              val levelPrice          = correctPriceByTickSize(submitted.price, submitted.order.orderType, tickSize)
              val updatedOrderBook    = currentUpdates.orderBook.insert(levelPrice, submitted)
              val updatedLevelChanges = currentUpdates.levelChanges.add(levelPrice, submitted)
              currentUpdates.copy(orderBook = updatedOrderBook, levelChanges = updatedLevelChanges)
            case submitted: MarketOrder =>
              // Cancel market order in the absence of counters
              currentUpdates.copy(events = currentUpdates.events enqueue unmatchable(submitted))
          }
      }

    val initialUpdates = OrderBookUpdates(orderBook, events, LevelAmounts.empty, None)
    loop(submitted, initialUpdates)
  }

  private def formatSide(side: Side): String =
    side
      .map { case (price, level) => s""""$price":${level.map(formatLo).mkString("[", ",", "]")}""" }
      .mkString("{", ",", "}")

  // Showing owner for old orders. Should be deleted in Order.MaxLiveTime
  private def formatLo(lo: LimitOrder): String = s"""{"id":"${lo.order.id()}","owner":"${lo.order.senderPublicKey.toAddress.stringRepr}"}"""

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  val empty: OrderBook = new OrderBook(TreeMap.empty(bidsOrdering), TreeMap.empty(asksOrdering), None, HashMap.empty)

  private def transformSide(side: OrderBookSideSnapshot, expectedSide: OrderType, ordering: Ordering[Long]): Side = {
    var bidMap = TreeMap.empty[Price, Level](ordering)
    for ((p, level) <- side) {
      var v = Queue.empty[LimitOrder]
      for (lo <- level) {
        require(lo.order.orderType == expectedSide,
                s"Expecting $expectedSide only orders in bid list, but ${lo.order.id()} is a ${lo.order.orderType} order")
        v = v.enqueue(lo)
      }
      bidMap += p -> v
    }
    bidMap
  }

  def apply(snapshot: OrderBookSnapshot): OrderBook = new OrderBook(
    transformSide(snapshot.bids, OrderType.BUY, bidsOrdering),
    transformSide(snapshot.asks, OrderType.SELL, asksOrdering),
    snapshot.lastTrade,
    orderIds(snapshot.asks, snapshot.bids)
  )

  def orderIds(asks: OrderBookSideSnapshot, bids: OrderBookSideSnapshot): HashMap[Order.Id, (OrderType, Price)] = {
    val r = HashMap.newBuilder[Order.Id, (OrderType, Price)]

    for {
      (price, level) <- asks.iterator ++ bids.iterator
      lo             <- level
    } r.+=((lo.order.id(), (lo.order.orderType, price))) // The compiler doesn't allow write this less ugly

    r.result()
  }

  def overlaps(submitted: AcceptedOrder, levelPrice: Price): Boolean = overlaps(submitted.order, levelPrice)
  def overlaps(submitted: Order, levelPrice: Price): Boolean =
    submitted.orderType.askBid(submitted.price <= levelPrice, submitted.price >= levelPrice)

  case class OrderBookUpdates(orderBook: OrderBook, events: Queue[Event], levelChanges: LevelAmounts, lastTrade: Option[LastTrade])
}
