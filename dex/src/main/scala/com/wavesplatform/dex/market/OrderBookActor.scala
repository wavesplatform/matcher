package com.wavesplatform.dex.market

import java.io.EOFException

import akka.actor.typed
import akka.actor.typed.scaladsl.adapter._
import akka.{actor => classic}
import cats.data.NonEmptyList
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.market.MatcherActor.{AddWsSubscription, ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.wavesplatform.dex.market.OrderBookActor._
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCancelFailed}
import com.wavesplatform.dex.model.{LastTrade, _}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule}
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.util.WorkingStash
import com.wavesplatform.dex.{OrderBookWsState, error}
import kamon.Kamon
import mouse.any._
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class OrderBookActor(settings: Settings,
                     owner: classic.ActorRef,
                     addressActor: classic.ActorRef,
                     snapshotStore: classic.ActorRef,
                     assetPair: AssetPair,
                     time: Time,
                     wsUpdates: WsOrderBook.Update,
                     var matchingRules: NonEmptyList[DenormalizedMatchingRule],
                     updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
                     normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule,
                     getMakerTakerFeeByOffset: Long => (AcceptedOrder, LimitOrder) => (Long, Long))(implicit ec: ExecutionContext)
    extends classic.Actor
    with WorkingStash
    with ScorexLogging {

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  // TODO context watch
  private var aggregated: typed.ActorRef[AggregatedOrderBookActor.Message] = _

  private var savingSnapshot          = Option.empty[QueueEventWithMeta.Offset]
  private var lastSavedSnapshotOffset = Option.empty[QueueEventWithMeta.Offset]
  private var lastProcessedOffset     = Option.empty[QueueEventWithMeta.Offset]

  private val addTimer    = Kamon.timer("matcher.orderbook.add").refine("pair" -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.cancel").refine("pair" -> assetPair.toString)
  private var orderBook   = OrderBook.empty

  private var actualRule: MatchingRule = normalizeMatchingRule(matchingRules.head)

  private var wsState = OrderBookWsState(wsUpdates, Queue.empty, WsOrderBook.empty)

  private def actualizeRules(offset: QueueEventWithMeta.Offset): Unit = {
    val actualRules = DenormalizedMatchingRule.skipOutdated(offset, matchingRules)
    if (matchingRules.head != actualRules.head) {
      matchingRules = actualRules
      updateCurrentMatchingRules(matchingRules.head)
      actualRule = normalizeMatchingRule(matchingRules.head)
    }
  }

  private val recovering: Receive = {
    case OrderBookSnapshotStoreActor.Response.GetSnapshot(result) =>
      result.foreach { case (_, snapshot) => orderBook = OrderBook(snapshot) }

      lastSavedSnapshotOffset = result.map(_._1)
      lastProcessedOffset = lastSavedSnapshotOffset

      log.debug(
        lastSavedSnapshotOffset match {
          case None    => "Recovery completed"
          case Some(x) => s"Recovery completed at $x: $orderBook"
        }
      )

      lastProcessedOffset foreach actualizeRules

      // TODO
      aggregated = context.spawn(AggregatedOrderBookActor(assetPair, 8, 8, AggregatedOrderBookActor.State.fromOrderBook(orderBook)), "aggregated")
      processEvents(orderBook.allOrders.map(lo => OrderAdded(lo, lo.order.timestamp)))

      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)
      context.become(working)
      unstashAll()

    case x => stash(x)
  }

  private val working: Receive = {
    case request: QueueEventWithMeta =>
      actualizeRules(request.offset)
      lastProcessedOffset match {
        case Some(lastProcessed) if request.offset <= lastProcessed => // Already processed
        case _ =>
          lastProcessedOffset = Some(request.offset)
          request.event match {
            case QueueEvent.Placed(limitOrder)        => onAddOrder(request, limitOrder)
            case QueueEvent.PlacedMarket(marketOrder) => onAddOrder(request, marketOrder)
            case x: QueueEvent.Canceled               => onCancelOrder(request, x.orderId)
            case _: QueueEvent.OrderBookDeleted =>
              wsState.wsConnections.foreach(_ ! wsCloseMessage)
              wsState = wsState.withoutSubscriptions
              process(orderBook.cancelAll(request.timestamp))
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

    case _: AddWsSubscription =>
      if (!wsState.hasSubscriptions) scheduleNextSendWsUpdates()
      wsState = wsState.addSubscription(sender)
      sender ! wsSnapshotOf(orderBook)
      log.trace(s"[${sender.hashCode()}] WebSocket connected")
      context.watch(sender)

    case SendWsUpdates =>
      wsState = wsState.flushed()
      if (wsState.hasSubscriptions) scheduleNextSendWsUpdates()

    case x: AggregatedOrderBookActor.Message => aggregated.tell(x)

    case classic.Terminated(ws) =>
      if (ws == aggregated) log.error("Terminated aggregated actor!")
      else {
        log.trace(s"[${ws.hashCode()}] WebSocket terminated")
        wsState = wsState.withoutSubscription(ws)
      }
  }

  override val receive: Receive = recovering

  private def process(result: (OrderBook, TraversableOnce[Event], LevelAmounts)): Unit = {
    val (updatedOrderBook, events, levelChanges) = result
    orderBook = updatedOrderBook
    log.info(s"Level changes: $levelChanges, before wsState.changes: ${wsState.changes}")
    wsState = wsState.withLevelChanges(levelChanges)
    val hasTrades = events.exists {
      case _: Events.OrderExecuted => true
      case _                       => false
    }
    if (hasTrades) orderBook.lastTrade.map(wsState.withLastTrade).foreach(wsState = _)
    log.info(s"after wsState.changes: ${wsState.changes}")
    aggregated ! AggregatedOrderBookActor.Command.ApplyChanges(levelChanges, orderBook.lastTrade, System.currentTimeMillis()) // TODO
    processEvents(events)
  }

  private def processEvents(events: TraversableOnce[Event]): Unit = events.foreach(addressActor ! _.unsafeTap(logEvent))

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
      case (updatedOrderBook, Some(cancelEvent), levelChanges) =>
        // TODO replace by process() in Scala 2.13
        orderBook = updatedOrderBook
        log.info(s"Level changes: $levelChanges, before wsState.changes: ${wsState.changes}")
        wsState = wsState.withLevelChanges(levelChanges)
        log.info(s"after wsState.changes: ${wsState.changes}")
        aggregated ! AggregatedOrderBookActor.Command.ApplyChanges(levelChanges, orderBook.lastTrade, cancelEvent.timestamp)
        processEvents(List(cancelEvent))
      case _ =>
        log.warn(s"Error applying $event: order not found")
        addressActor ! OrderCancelFailed(id, error.OrderNotFound(id))
    }
  }

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, acceptedOrder: AcceptedOrder): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    process(orderBook.add(acceptedOrder, eventWithMeta.timestamp, getMakerTakerFeeByOffset(eventWithMeta.offset), actualRule.tickSize))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def wsSnapshotOf(ob: OrderBook): WsOrderBook = wsUpdates.from(
    asks = ob.asks.aggregated,
    bids = ob.bids.aggregated,
    lt = ob.lastTrade
  )

  private def scheduleNextSendWsUpdates(): classic.Cancellable =
    context.system.scheduler.scheduleOnce(settings.wsMessagesInterval, self, SendWsUpdates)

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

  private val reason = new EOFException("Order book was deleted")
  reason.setStackTrace(Array.empty)

  private val wsCloseMessage = classic.Status.Failure(reason)

  case class Settings(wsMessagesInterval: FiniteDuration)

  def props(settings: Settings,
            parent: classic.ActorRef,
            addressActor: classic.ActorRef,
            snapshotStore: classic.ActorRef,
            assetPair: AssetPair,
            time: Time,
            wsUpdates: WsOrderBook.Update,
            matchingRules: NonEmptyList[DenormalizedMatchingRule],
            updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
            normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule,
            getMakerTakerFeeByOffset: Long => (AcceptedOrder, LimitOrder) => (Long, Long))(implicit ec: ExecutionContext): classic.Props =
    classic.Props(
      new OrderBookActor(
        settings,
        parent,
        addressActor,
        snapshotStore,
        assetPair,
        time,
        wsUpdates,
        matchingRules,
        updateCurrentMatchingRules,
        normalizeMatchingRule,
        getMakerTakerFeeByOffset
      )
    )

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[LevelAgg],
      bestAsk: Option[LevelAgg]
  )

  object MarketStatus {
    implicit val marketStatusWrites: OWrites[MarketStatus] = { ms =>
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

    def apply(ob: OrderBook): MarketStatus = MarketStatus(ob.lastTrade, ob.bestBid, ob.bestAsk)
  }

  case class Snapshot(eventNr: Option[Long], orderBook: OrderBookSnapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdateCompleted(assetPair: AssetPair, currentOffset: Option[Long])
  case object SendWsUpdates
}
