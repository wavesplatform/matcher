package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.settings.MatchingRule
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.{HashMap, Queue, TreeMap}

case class OrderBook private (bids: Side, asks: Side, lastTrade: Option[LastTrade], orderIds: HashMap[Order.Id, (OrderType, Price)]) {
  import OrderBook._

  def bestBid: Option[LevelAgg] = bids.bestLevel
  def bestAsk: Option[LevelAgg] = asks.bestLevel

  def allOrders: Iterable[(Price, LimitOrder)] =
    for {
      (price, level) <- (bids: Iterable[(Price, Level)]) ++ asks
      lo             <- level
    } yield price -> lo

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

  def cancelAll(ts: Long): (OrderBook, Seq[OrderCanceled]) = {
    val canceledOrders = allOrders.map { case (_, lo) => OrderCanceled(lo, isSystemCancel = false, ts) }.toSeq
    (OrderBook.empty, canceledOrders)
  }

  def add(ao: AcceptedOrder,
          ts: Long,
          getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long),
          tickSize: Long = MatchingRule.DefaultRule.tickSize): (OrderBook, Queue[Event]) =
    doMatch(ts, tickSize, getMakerTakerFee, ao, this)

  def snapshot: OrderBookSnapshot                     = OrderBookSnapshot(bids, asks, lastTrade)
  def aggregatedSnapshot: OrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString: String = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""

  def withoutBest(tpe: OrderType): Option[(OrderBook, (Price, LimitOrder))] = tpe.askBid(
    asks.withoutBest.map { case (side, x) => (copy(asks = side, orderIds = orderIds - x._2.order.id()), x) },
    bids.withoutBest.map { case (side, x) => (copy(bids = side, orderIds = orderIds - x._2.order.id()), x) },
  )

  def insert(levelPrice: Price, lo: LimitOrder): OrderBook = lo.order.orderType.askBid(
    copy(asks = asks.put(levelPrice, lo), orderIds = orderIds.updated(lo.order.id(), (lo.order.orderType, levelPrice))),
    copy(bids = bids.put(levelPrice, lo), orderIds = orderIds.updated(lo.order.id(), (lo.order.orderType, levelPrice)))
  )

  // TODO orderIds
  def prependBest(levelPrice: Price, lo: LimitOrder): OrderBook = lo.order.orderType.askBid(
    copy(asks = asks.updated(levelPrice, lo +: asks.getOrElse(levelPrice, Queue.empty)),
         orderIds = orderIds.updated(lo.order.id(), (lo.order.orderType, levelPrice))),
    copy(bids = bids.updated(levelPrice, lo +: bids.getOrElse(levelPrice, Queue.empty)),
         orderIds = orderIds.updated(lo.order.id(), (lo.order.orderType, levelPrice)))
  )
}

object OrderBook {

  final implicit class OrderTypeOps(val self: OrderType) extends AnyVal {
    def askBid[T](ifAsk: => T, ifBid: => T): T = if (self == OrderType.SELL) ifAsk else ifBid
    def opposite: OrderType                    = askBid(OrderType.BUY, OrderType.SELL)
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
                      getMakerTakerMaxFee: (AcceptedOrder, LimitOrder) => (Long, Long),
                      submitted: AcceptedOrder,
                      orderBook: OrderBook): (OrderBook, Queue[Event]) = {
    @scala.annotation.tailrec
    def loop(orderBook: OrderBook, submitted: AcceptedOrder, events: Queue[Event]): (OrderBook, Queue[Event]) =
      if (!submitted.order.isValid(eventTs)) (orderBook, events.enqueue(OrderCanceled(submitted, isSystemCancel = false, eventTs)))
      else
        orderBook.withoutBest(submitted.order.orderType.opposite) match {
          case Some((orderBook, (levelPrice, counter))) if overlaps(submitted, levelPrice) =>
            if (!counter.order.isValid(eventTs))
              loop(
                orderBook = orderBook,
                submitted = submitted,
                events = events.enqueue(OrderCanceled(counter, isSystemCancel = false, eventTs))
              )
            else {
              val (maxCounterFee, maxSubmittedFee) = getMakerTakerMaxFee(submitted, counter)
              val orderExecutedEvent               = OrderExecuted(submitted, counter, eventTs, maxSubmittedFee, maxCounterFee)
              val updatedEvents                    = events.enqueue(orderExecutedEvent)
              val updatedOrderBook =
                orderBook.copy(lastTrade = Some(LastTrade(counter.price, orderExecutedEvent.executedAmount, submitted.order.orderType)))

              val counterRemaining   = orderExecutedEvent.counterRemaining
              val submittedRemaining = orderExecutedEvent.submittedRemaining
              if (counterRemaining.isValid) { // counter is not filled
                // if submitted is not filled (e.g. LimitOrder: rounding issues, MarkerOrder: afs = 0) cancel its remaining
                if (submittedRemaining.isValid)
                  (
                    updatedOrderBook.prependBest(levelPrice, counterRemaining),
                    updatedEvents.enqueue(OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs))
                  )
                else (updatedOrderBook, updatedEvents)
              } else { // counter is filled
                submittedRemaining match {
                  case _: LimitOrder =>
                    if (submittedRemaining.isValid) loop(updatedOrderBook, submittedRemaining, updatedEvents)
                    else (updatedOrderBook, updatedEvents)

                  case submittedRemaining: MarketOrder =>
                    val isSubmittedFilled = !submittedRemaining.isValid
                    val canSpendMore      = submittedRemaining.availableForSpending > 0
                    (isSubmittedFilled, canSpendMore) match {
                      case (true, _)     => (updatedOrderBook, updatedEvents)
                      case (false, true) => loop(updatedOrderBook, submittedRemaining, updatedEvents)
                      case (false, false) =>
                        (updatedOrderBook, updatedEvents.enqueue(OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs)))
                    }
                }
              }
            }

          case _ =>
            submitted match {
              case submitted: LimitOrder =>
                val levelPrice = correctPriceByTickSize(submitted.price, submitted.order.orderType, tickSize)
                (orderBook.insert(levelPrice, submitted), events.enqueue(OrderAdded(submitted, eventTs)))
              case submitted: MarketOrder =>
                // Cancel market order in the absence of counters
                (orderBook, events.enqueue(OrderCanceled(submitted, isSystemCancel = true, eventTs)))
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

  def orderIds(asks: OrderBookSideSnapshot, bids: OrderBookSideSnapshot): HashMap[Order.Id, (OrderType, Price)] = {
    val r = HashMap.newBuilder[Order.Id, (OrderType, Price)]

    for {
      (price, level) <- (bids: Iterable[(Price, Seq[LimitOrder])]) ++ asks
      lo             <- level
    } r.+=((lo.order.id(), (lo.order.orderType, price))) // The compiler doesn't allow write this less ugly

    r.result()
  }

  def overlaps(submitted: AcceptedOrder, levelPrice: Price): Boolean = overlaps(submitted.order, levelPrice)
  def overlaps(submitted: Order, levelPrice: Price): Boolean =
    submitted.orderType.askBid(submitted.price <= levelPrice, submitted.price >= levelPrice)
}
