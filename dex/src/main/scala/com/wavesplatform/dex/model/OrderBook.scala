package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events.{Event, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.settings.MatchingRule
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.{HashMap, Queue, TreeMap}
import scala.collection.mutable

case class OrderBook private (bids: Side, asks: Side, lastTrade: Option[LastTrade], orderIds: HashMap[Order.Id, (OrderType, Price)]) {
  import OrderBook._

  // Only for tests, do not use in production
  private[model] def getBids: Side = bids
  private[model] def getAsks: Side = asks

  def bestBid: Option[LevelAgg]       = bids.bestLevel
  def bestAsk: Option[LevelAgg]       = asks.bestLevel
  def getLastTrade: Option[LastTrade] = lastTrade

  def allOrders: Iterable[(Price, LimitOrder)] = {
    for {
      (price, level) <- (bids: Iterable[(Price, Level)]) ++ asks
      lo             <- level
    } yield price -> lo
  }

  def cancel(orderId: ByteStr, timestamp: Long): (OrderBook, Option[OrderCanceled]) =
    orderIds
      .get(orderId)
      .map {
        case (orderType, price) =>
          if (orderType == OrderType.SELL) {
            val (updatedAsks, lo) = asks.remove(price, orderId)
            (copy(asks = updatedAsks, orderIds = orderIds - orderId), Some(OrderCanceled(lo, isSystemCancel = false, timestamp)))
          } else {
            val (updatedBids, lo) = bids.remove(price, orderId)
            (copy(bids = updatedBids, orderIds = orderIds - orderId), Some(OrderCanceled(lo, isSystemCancel = false, timestamp)))
          }
      }
      .getOrElse((this, Option.empty[OrderCanceled]))

  def cancelAll(timestamp: Long): (OrderBook, Seq[OrderCanceled]) = {
    val canceledOrders = allOrders.map { case (_, lo) => OrderCanceled(lo, isSystemCancel = false, timestamp) }.toSeq
    (OrderBook.empty, canceledOrders)
  }

  def add(ao: AcceptedOrder,
          ts: Long,
          getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long),
          tickSize: Long = MatchingRule.DefaultRule.tickSize): (OrderBook, Queue[Event]) =
    ao.order.orderType match {
      case OrderType.BUY  => doMatch(ts, tickSize, getMakerTakerFee, ao, this, Queue.empty)
      case OrderType.SELL => doMatch(ts, tickSize, getMakerTakerFee, ao, this, Queue.empty)
    }

  def snapshot: OrderBookSnapshot                     = OrderBookSnapshot(bids, asks, lastTrade)
  def aggregatedSnapshot: OrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString: String = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""

  def best(tpe: OrderType): Option[(Price, LimitOrder)] = side(tpe).best
  def side(tpe: OrderType): Side                        = tpe.askBid(asks, bids)
  def removeBest(tpe: OrderType): OrderBook = tpe.askBid(
    copy(asks = asks.removeBest()._1),
    copy(bids = bids.removeBest()._1)
  )

  def replaceBest(lo: LimitOrder): (OrderBook, LimitOrder) = lo.order.orderType.askBid(
    asks.replaceBest(lo).copy(copy(asks = _)),
    asks.replaceBest(lo).copy(copy(bids = _))
  )

  def overlaps(counter: LimitOrder, submitted: AcceptedOrder): Boolean = overlaps(counter.order, submitted.order)
  def overlaps(counter: Order, submitted: Order): Boolean =
    submitted.orderType.askBid(submitted.price <= counter.price, submitted.price >= counter.price)
}

object OrderBook {

  final implicit class OrderTypeOps(val self: OrderType) extends AnyVal {
    def askBid[T](ifAsk: => T, ifBid: => T): T = if (self == OrderType.SELL) ifAsk else ifBid
  }

  /** Returns true if submitted buy order can be matched with counter sell order */
  private object canMatchBuy extends ((Long, Long) => Boolean) {
    def apply(submittedOrderPrice: Long, counterLevelPrice: Long): Boolean = submittedOrderPrice >= counterLevelPrice
    override val toString                                                  = "submitted >= counter"
  }

  /** Returns true if submitted sell order can be matched with counter buy order */
  private object canMatchSell extends ((Long, Long) => Boolean) {
    def apply(submittedOrderPrice: Long, counterLevelPrice: Long): Boolean = submittedOrderPrice <= counterLevelPrice
    override val toString                                                  = "submitted <= counter"
  }

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

  private def doMatch(eventTs: Long,
                      tickSize: Long,
                      getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long),
                      submitted: AcceptedOrder,
                      orderBook: OrderBook,
                      prevEvents: Queue[Event]): (OrderBook, Queue[Event]) = {
    @scala.annotation.tailrec
    def loop(orderBook: OrderBook, submitted: AcceptedOrder, prevEvents: Queue[Event]): (OrderBook, Queue[Event]) =
      if (!submitted.order.isValid(eventTs)) (orderBook, prevEvents.enqueue(OrderCanceled(submitted, isSystemCancel = false, eventTs)))
      else
        orderBook.best(submitted.order.orderType) match {
          case Some((counterPrice, counter)) if orderBook.overlaps(counter, submitted) =>
            if (!counter.order.isValid(eventTs))
              loop(
                orderBook = orderBook.removeBest(counter.order.orderType),
                submitted = submitted,
                prevEvents = prevEvents.enqueue(OrderCanceled(counter, isSystemCancel = false, eventTs))
              )
            else {
              val (maxCounterFee, maxSubmittedFee) = getMakerTakerFee(submitted, counter)
              val orderExecutedEvent               = OrderExecuted(submitted, counter, eventTs, maxSubmittedFee, maxCounterFee)
              val updatedEvents                    = prevEvents.enqueue(orderExecutedEvent)
              val updatedOrderBook =
                orderBook.copy(lastTrade = Some(LastTrade(counter.price, orderExecutedEvent.executedAmount, submitted.order.orderType)))

              val counterRemaining = orderExecutedEvent.counterRemaining
              if (counterRemaining.isValid) { // counter is not filled
                val submittedRemaining = orderExecutedEvent.submittedRemaining
                // if submitted is not filled (e.g. LimitOrder: rounding issues, MarkerOrder: afs = 0) cancel its remaining
                if (submittedRemaining.isValid)
                  (
                    updatedOrderBook.replaceBest(counterRemaining)._1,
                    updatedEvents.enqueue(OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs))
                  )
                else (updatedOrderBook, updatedEvents)
              } else { // counter is filled
                val updatedOrderBook2 = updatedOrderBook.removeBest(counter.order.orderType)
                submitted match {
                  case _: LimitOrder =>
                    val submittedRemaining = orderExecutedEvent.submittedRemaining
                    if (submittedRemaining.isValid) loop(updatedOrderBook2, submittedRemaining, updatedEvents)
                    else (updatedOrderBook2, updatedEvents)

                  case submitted: MarketOrder =>
                    val submittedRemaining = orderExecutedEvent.submittedMarketRemaining(submitted)
                    val isSubmittedFilled  = !submittedRemaining.isValid
                    val canSpendMore       = submittedRemaining.availableForSpending > 0

                    (isSubmittedFilled, canSpendMore) match {
                      case (true, _)     => (updatedOrderBook2, updatedEvents)
                      case (false, true) => loop(updatedOrderBook2, submittedRemaining, updatedEvents)
                      case (false, false) =>
                        (updatedOrderBook2, updatedEvents.enqueue(OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs)))
                    }
                }
              }
            }
        }

    loop(orderBook, submitted, Queue.empty)
  }

  private def formatSide(side: Side): String =
    side
      .map { case (price, level) => s""""$price":${level.map(formatLo).mkString("[", ",", "]")}""" }
      .mkString("{", ",", "}")

  // Showing owner for old orders. Should be deleted in Order.MaxLiveTime
  private def formatLo(lo: LimitOrder): String = s"""{"id":"${lo.order.id()}","owner":"${lo.order.senderPublicKey.toAddress.stringRepr}"}"""

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  private def limitOrder(remainingAmount: Long, remainingFee: Long, o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(remainingAmount, remainingFee, o)
    case OrderType.SELL => SellLimitOrder(remainingAmount, remainingFee, o)
  }

  private implicit val limitOrderFormat: Format[LimitOrder] = Format(
    Reads[LimitOrder] {
      case js: JsObject =>
        val amount = (js \ "amount").as[Long]
        val order  = (js \ "order").as[Order]
        val fee    = (js \ "fee").asOpt[Long].getOrElse(AcceptedOrder.partialFee(order.matcherFee, order.amount, amount))
        JsSuccess(limitOrder(amount, fee, order))
      case _ => JsError("failed to deserialize LimitOrder")
    },
    ((__ \ "amount").format[Long] and
      (__ \ "fee").format[Long] and
      (__ \ "order").format[Order])(limitOrder, (lo: LimitOrder) => (lo.amount, lo.fee, lo.order))
  )

  implicit val priceMapFormat: Format[OrderBookSideSnapshot] =
    implicitly[Format[Map[String, Seq[LimitOrder]]]].inmap(
      _.map { case (k, v) => k.toLong   -> v },
      _.map { case (k, v) => k.toString -> v }
    )

  implicit val snapshotFormat: Format[OrderBookSnapshot] = Json.format

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

  def orderIds(asks: Side, bids: Side): HashMap[Order.Id, (OrderType, Price)] = {
    val r = HashMap.newBuilder[Order.Id, (OrderType, Price)]

    for {
      (price, level) <- (bids: Iterable[(Price, Level)]) ++ asks
      lo             <- level
    } r.+=((lo.order.id(), (lo.order.orderType, price))) // The compiler doesn't allow write this less ugly

    r.result()
  }
}
