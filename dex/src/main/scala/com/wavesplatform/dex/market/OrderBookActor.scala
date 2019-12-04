package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef, Props}
import cats.data.NonEmptyList
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.wavesplatform.dex.api._
import com.wavesplatform.dex.error
import com.wavesplatform.dex.market.MatcherActor.{ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.wavesplatform.dex.market.OrderBookActor._
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCancelFailed}
import com.wavesplatform.dex.model.OrderBook.LastTrade
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatcherSettings, MatchingRule}
import com.wavesplatform.dex.util.WorkingStash
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import kamon.Kamon
import mouse.any._
import org.slf4j.LoggerFactory
import play.api.libs.json._

class OrderBookActor(owner: ActorRef,
                     addressActor: ActorRef,
                     snapshotStore: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     time: Time,
                     var matchingRules: NonEmptyList[DenormalizedMatchingRule],
                     updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
                     normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  private var savingSnapshot          = Option.empty[QueueEventWithMeta.Offset]
  private var lastSavedSnapshotOffset = Option.empty[QueueEventWithMeta.Offset]
  private var lastProcessedOffset     = Option.empty[QueueEventWithMeta.Offset]

  private val addTimer    = Kamon.timer("matcher.orderbook.add").refine("pair" -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.cancel").refine("pair" -> assetPair.toString)
  private var orderBook   = OrderBook.empty

  private var actualRule: MatchingRule = normalizeMatchingRule(matchingRules.head)

  private def actualizeRules(offset: QueueEventWithMeta.Offset): Unit = {
    val actualRules = DenormalizedMatchingRule.skipOutdated(offset, matchingRules)
    if (matchingRules.head != actualRules.head) {
      matchingRules = actualRules
      updateCurrentMatchingRules(matchingRules.head)
      actualRule = normalizeMatchingRule(matchingRules.head)
    }
  }

  override def receive: Receive = recovering

  private def recovering: Receive = {
    case OrderBookSnapshotStoreActor.Response.GetSnapshot(result) =>
      result.foreach { case (_, snapshot) => orderBook = OrderBook(snapshot) }

      lastSavedSnapshotOffset = result.map(_._1)
      lastProcessedOffset = lastSavedSnapshotOffset

      log.debug(lastSavedSnapshotOffset match {
        case None    => "Recovery completed"
        case Some(x) => s"Recovery completed at $x: $orderBook"
      })

      lastProcessedOffset foreach actualizeRules

      updateMarketStatus(MarketStatus(orderBook))
      updateSnapshot(orderBook.aggregatedSnapshot)
      processEvents(orderBook.allOrders.map { case (_, lo) => OrderAdded(lo, lo.order.timestamp) })

      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)
      context.become(working)
      unstashAll()

    case x => stash(x)
  }

  private def working: Receive = {
    case request: QueueEventWithMeta =>
      actualizeRules(request.offset)
      lastProcessedOffset match {
        case Some(lastProcessed) if request.offset <= lastProcessed => sender() ! AlreadyProcessed
        case _ =>
          lastProcessedOffset = Some(request.offset)
          request.event match {
            case QueueEvent.Placed(limitOrder)        => onAddOrder(request, limitOrder)
            case QueueEvent.PlacedMarket(marketOrder) => onAddOrder(request, marketOrder)
            case x: QueueEvent.Canceled               => onCancelOrder(request, x.orderId)
            case _: QueueEvent.OrderBookDeleted =>
              updateSnapshot(OrderBook.AggregatedSnapshot())
              processEvents(orderBook.cancelAll(request.timestamp))
              // We don't delete the snapshot, because it could be required after restart
              // snapshotStore ! OrderBookSnapshotStoreActor.Message.Delete(assetPair)
              context.stop(self)
          }
      }

    case MatcherActor.Ping => sender() ! MatcherActor.Pong

    case ForceStartOrderBook(p) if p == assetPair => sender() ! OrderBookCreated(assetPair)

    case OrderBookSnapshotStoreActor.Response.Updated(offset) =>
      log.info(s"Snapshot has been saved at offset $offset")
      lastSavedSnapshotOffset = Some(offset)
      owner ! OrderBookSnapshotUpdateCompleted(assetPair, lastSavedSnapshotOffset)
      savingSnapshot = None

    case SaveSnapshot(globalEventNr) =>
      if (savingSnapshot.isEmpty && lastSavedSnapshotOffset.getOrElse(-1L) < globalEventNr) {
        saveSnapshotAt(globalEventNr)
        savingSnapshot = Some(globalEventNr)
      }
  }

  private def processEvents(events: Iterable[Event]): Unit = {
    updateMarketStatus(MarketStatus(orderBook))
    updateSnapshot(orderBook.aggregatedSnapshot)
    events.foreach(addressActor ! _.unsafeTap(logEvent))
  }

  private def logEvent(e: Event): Unit = log.info {
    import Events._
    e match {
      case e: OrderAdded    => s"OrderAdded(${e.order.order.id()}, amount=${e.order.amount})"
      case e: OrderCanceled => s"OrderCanceled(${e.acceptedOrder.order.id()}, system=${e.isSystemCancel})"
      case e: OrderExecuted => s"OrderExecuted(s=${e.submitted.order.id()}, c=${e.counter.order.id()}, amount=${e.executedAmount}, ts=${e.timestamp})"
    }
  }

  private def onCancelOrder(event: QueueEventWithMeta, id: Order.Id): Unit = cancelTimer.measure {
    orderBook.cancel(id, event.timestamp) match {
      case Some(cancelEvent) => processEvents(List(cancelEvent))
      case None =>
        log.warn(s"Error applying $event: order not found")
        addressActor ! OrderCancelFailed(id, error.OrderNotFound(id))
    }
  }

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, acceptedOrder: AcceptedOrder): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    processEvents(orderBook.add(acceptedOrder, eventWithMeta.timestamp, actualRule.tickSize))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def saveSnapshotAt(globalEventNr: QueueEventWithMeta.Offset): Unit = {
    val saveSnapshot = (lastSavedSnapshotOffset, lastProcessedOffset).tupled.forall { case (saved, processed) => saved < processed }
    val toSave       = if (saveSnapshot) Some(orderBook.snapshot) else None

    if (saveSnapshot) {
      log.trace(s"About to save snapshot $orderBook")
      log.debug(
        s"Saving both offset and snapshot. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    } else {
      log.debug(s"Saving offset only. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    }

    snapshotStore ! OrderBookSnapshotStoreActor.Message.Update(assetPair, globalEventNr, toSave)
  }

  snapshotStore ! OrderBookSnapshotStoreActor.Message.GetSnapshot(assetPair)
}

object OrderBookActor {

  def props(parent: ActorRef,
            addressActor: ActorRef,
            snapshotStore: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
            updateMarketStatus: MarketStatus => Unit,
            settings: MatcherSettings,
            time: Time,
            matchingRules: NonEmptyList[DenormalizedMatchingRule],
            updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
            normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule): Props =
    Props(
      new OrderBookActor(
        parent,
        addressActor,
        snapshotStore,
        assetPair,
        updateSnapshot,
        updateMarketStatus,
        time,
        matchingRules,
        updateCurrentMatchingRules,
        normalizeMatchingRule,
      )
    )

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[LevelAgg],
      bestAsk: Option[LevelAgg],
  )

  object MarketStatus {
    implicit val fmt: Writes[MarketStatus] = { ms =>
      Json.obj(
        "lastPrice"  -> ms.lastTrade.map(_.price),
        "lastAmount" -> ms.lastTrade.map(_.amount),
        "lastSide"   -> ms.lastTrade.map(_.side.toString),
        "bid"        -> ms.bestBid.map(_.price),
        "bidAmount"  -> ms.bestBid.map(_.amount),
        "ask"        -> ms.bestAsk.map(_.price),
        "askAmount"  -> ms.bestAsk.map(_.amount)
      )
    }

    def apply(ob: OrderBook): MarketStatus = MarketStatus(ob.getLastTrade, ob.bestBid, ob.bestAsk)
  }

  case class Snapshot(eventNr: Option[Long], orderBook: OrderBook.Snapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdateCompleted(assetPair: AssetPair, currentOffset: Option[Long])
}
