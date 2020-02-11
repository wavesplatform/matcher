package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.settings.MatchingRule
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable

class OrderBook private (private[OrderBook] val bids: Side, private[OrderBook] val asks: Side, private[OrderBook] var lastTrade: Option[LastTrade]) {
  import OrderBook._

  private[model] def getBids: Side = bids
  private[model] def getAsks: Side = asks

  def bestBid: Option[LevelAgg]       = bids.aggregated.headOption
  def bestAsk: Option[LevelAgg]       = asks.aggregated.headOption
  def getLastTrade: Option[LastTrade] = lastTrade

  def allOrders: Iterable[(Long, LimitOrder)] = {
    for {
      (price, level) <- (bids: Iterable[(Price, Level)]) ++ asks
      lo             <- level
    } yield price -> lo
  }

  def cancel(orderId: ByteStr, timestamp: Long): Option[OrderCanceled] = {
    allOrders.collectFirst {
      case (price, lo) if lo.order.id() == orderId =>
        (if (lo.order.orderType == OrderType.BUY) bids else asks).remove(price, lo.order.id())
        OrderCanceled(lo, isSystemCancel = false, timestamp)
    }
  }

  def cancelAll(timestamp: Long): Seq[OrderCanceled] = {
    val canceledOrders = allOrders.map { case (_, lo) => OrderCanceled(lo, isSystemCancel = false, timestamp) }.toSeq
    bids.clear()
    asks.clear()
    canceledOrders
  }

  def add(ao: AcceptedOrder,
          ts: Long,
          getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long),
          tickSize: Long = MatchingRule.DefaultRule.tickSize): Seq[Event] = {

    val (events, lt) = ao.order.orderType match {
      case OrderType.BUY  => doMatch(ts, canMatchBuy, ao, Seq.empty, bids, asks, lastTrade, tickSize, getMakerTakerFee)
      case OrderType.SELL => doMatch(ts, canMatchSell, ao, Seq.empty, asks, bids, lastTrade, tickSize, getMakerTakerFee)
    }

    lastTrade = lt
    events.reverse
  }

  def snapshot: OrderBookSnapshot                     = OrderBookSnapshot(bids.toMap, asks.toMap, lastTrade)
  def aggregatedSnapshot: OrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString: String = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""
}

object OrderBook {

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

  /** @param canMatch (submittedOrderPrice, counterLevelPrice) => Boolean */
  @tailrec
  private def doMatch(eventTs: Long,
                      canMatch: (Long, Long) => Boolean,
                      submitted: AcceptedOrder,
                      prevEvents: Seq[Event],
                      submittedSide: Side,
                      counterSide: Side,
                      lastTrade: Option[LastTrade],
                      tickSize: Long,
                      getMakerTakerFee: (AcceptedOrder, LimitOrder) => (Long, Long)): (Seq[Event], Option[LastTrade]) = {
    if (!submitted.order.isValid(eventTs)) (OrderCanceled(submitted, isSystemCancel = false, eventTs) +: prevEvents, lastTrade)
    else {
      counterSide.best match {
        case counterAndItsLevelPrice if counterAndItsLevelPrice.forall { case (_, levelPrice) => !canMatch(submitted.order.price, levelPrice) } =>
          submitted.fold { submittedLimitOrder =>
            // place limit order into appropriate level according to the tick size
            val correctedLevelPriceOfSubmittedOrder =
              correctPriceByTickSize(submittedLimitOrder.price, submittedLimitOrder.order.orderType, tickSize)

            submittedSide += correctedLevelPriceOfSubmittedOrder -> (submittedSide.getOrElse(correctedLevelPriceOfSubmittedOrder, Vector.empty) :+ submittedLimitOrder)
            (OrderAdded(submittedLimitOrder, eventTs) +: prevEvents, lastTrade)

          } { submittedMarketOrder =>
            // cancel market order in the absence of counters
            (OrderCanceled(submittedMarketOrder, isSystemCancel = true, eventTs) +: prevEvents, lastTrade)
          }
        case Some((counter, _)) =>
          if (!submitted.isValid(counter.price)) (OrderCanceled(submitted, isSystemCancel = true, eventTs) +: prevEvents, lastTrade)
          else if (!counter.order.isValid(eventTs)) {

            counterSide.removeBest()
            doMatch(
              eventTs,
              canMatch,
              submitted,
              OrderCanceled(counter, isSystemCancel = false, eventTs) +: prevEvents,
              submittedSide,
              counterSide,
              lastTrade,
              tickSize,
              getMakerTakerFee
            )

          } else {

            val (maxCounterFee, maxSubmittedFee) = getMakerTakerFee(submitted, counter)
            val orderExecutedEvent               = OrderExecuted(submitted, counter, eventTs, maxSubmittedFee, maxCounterFee)
            val newEvents                        = orderExecutedEvent +: prevEvents
            val lt                               = Some(LastTrade(counter.price, orderExecutedEvent.executedAmount, submitted.order.orderType))

            if (orderExecutedEvent.counterRemaining.isValid) { // counter is not filled

              counterSide.replaceBest(orderExecutedEvent.counterRemaining)
              val submittedRemaining = orderExecutedEvent.submittedRemaining
              // if submitted is not filled (e.g. LimitOrder: rounding issues, MarkerOrder: afs = 0) cancel its remaining
              if (submittedRemaining.isValid) (OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs) +: newEvents, lt)
              else (newEvents, lt)

            } else { // counter is filled

              counterSide.removeBest()

              submitted match {
                case _: LimitOrder =>
                  val remaining = orderExecutedEvent.submittedRemaining
                  if (remaining.isValid) doMatch(eventTs, canMatch, remaining, newEvents, submittedSide, counterSide, lt, tickSize, getMakerTakerFee)
                  else (newEvents, lt)

                case submittedMarketOrder: MarketOrder =>
                  val remaining         = orderExecutedEvent.submittedMarketRemaining(submittedMarketOrder)
                  val isSubmittedFilled = !remaining.isValid
                  val canSpendMore      = remaining.availableForSpending > 0

                  (isSubmittedFilled, canSpendMore) match {
                    case (false, true)  => doMatch(eventTs, canMatch, remaining, newEvents, submittedSide, counterSide, lt, tickSize, getMakerTakerFee)
                    case (false, false) => (OrderCanceled(remaining, isSystemCancel = true, eventTs) +: newEvents, lt)
                    case (true, _)      => (newEvents, lt)
                  }
              }
            }
          }
      }
    }
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

  def empty(): OrderBook = new OrderBook(mutable.TreeMap.empty(bidsOrdering), mutable.TreeMap.empty(asksOrdering), None)

  private def transformSide(side: OrderBookSideSnapshot, expectedSide: OrderType, ordering: Ordering[Long]): Side = {
    val bidMap = mutable.TreeMap.empty[Price, Level](ordering)
    for ((p, level) <- side) {
      val v = Vector.newBuilder[LimitOrder]
      for (lo <- level) {
        require(lo.order.orderType == expectedSide,
                s"Expecting $expectedSide only orders in bid list, but ${lo.order.id()} is a ${lo.order.orderType} order")
        v += lo
      }
      bidMap += p -> v.result()
    }
    bidMap
  }

  def apply(snapshot: OrderBookSnapshot): OrderBook =
    new OrderBook(transformSide(snapshot.bids, OrderType.BUY, bidsOrdering),
                  transformSide(snapshot.asks, OrderType.SELL, asksOrdering),
                  snapshot.lastTrade)
}
