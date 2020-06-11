package com.wavesplatform.dex.actors.orderbook

import akka.actor.typed
import akka.actor.typed.scaladsl.adapter._
import akka.{actor => classic}
import cats.data.NonEmptyList
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.wavesplatform.dex.actors.MatcherActor.{ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.wavesplatform.dex.actors.orderbook.OrderBookActor._
import com.wavesplatform.dex.actors.{MatcherActor, WorkingStash, orderbook}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.error
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCancelFailed}
import com.wavesplatform.dex.model.{LastTrade, _}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule, OrderRestrictionsSettings}
import com.wavesplatform.dex.time.Time
import kamon.Kamon
import mouse.any._
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

class OrderBookActor(settings: Settings,
                     owner: classic.ActorRef,
                     addressActor: classic.ActorRef,
                     snapshotStore: classic.ActorRef,
                     assetPair: AssetPair,
                     amountDecimals: Int,
                     priceDecimals: Int,
                     time: Time,
                     var matchingRules: NonEmptyList[DenormalizedMatchingRule],
                     updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
                     normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule,
                     getMakerTakerFeeByOffset: Long => (AcceptedOrder, LimitOrder) => (Long, Long),
                     restrictions: Option[OrderRestrictionsSettings])(implicit ec: ExecutionContext)
    extends classic.Actor
    with WorkingStash
    with ScorexLogging {

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  private var aggregatedRef: typed.ActorRef[AggregatedOrderBookActor.Message] = _

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
      // Could be unset during the start
      Option(aggregatedRef).foreach {
        _ ! AggregatedOrderBookActor.Command.ApplyChanges(
          LevelAmounts.empty,
          None,
          Some(matchingRules.head.tickSize.toDouble),
          System.currentTimeMillis
        )
      }
    }
  }

  override def receive: Receive = recovering

  private def recovering: Receive = {
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

      aggregatedRef = context.spawn(
        orderbook.AggregatedOrderBookActor(
          settings.aggregated,
          assetPair,
          amountDecimals,
          priceDecimals,
          restrictions,
          matchingRules.head.tickSize.toDouble,
          time,
          AggregatedOrderBookActor.State.fromOrderBook(orderBook)
        ),
        "aggregated"
      )
      context.watch(aggregatedRef)

      processEvents(orderBook.allOrders.map(lo => OrderAdded(lo, lo.order.timestamp)))

      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)
      context.become(working)
      unstashAll()

    case x => stash(x)
  }

  private def working: Receive = {
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
              process(request.timestamp, orderBook.cancelAll(request.timestamp))
              // We don't delete the snapshot, because it could be required after restart
              // snapshotStore ! OrderBookSnapshotStoreActor.Message.Delete(assetPair)
              aggregatedRef ! AggregatedOrderBookActor.Event.OrderBookRemoved
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

    case x: AggregatedOrderBookActor.Message => aggregatedRef.tell(x)

    case classic.Terminated(ref) =>
      log.error(s"Terminated actor: $ref")
      // If this happens the issue is critical and should not be handled. The order book will be stopped, see MatcherActor
      if (ref == aggregatedRef) throw new RuntimeException("Aggregated order book was terminated")
  }

  private def process(timestamp: Long, result: (OrderBook, TraversableOnce[Event], LevelAmounts)): Unit = {
    val (updatedOrderBook, events, levelChanges) = result
    orderBook = updatedOrderBook
    // DEX-712
    val hasTrades = events.exists {
      case _: Events.OrderExecuted => true
      case _                       => false
    }
    val lastTrade = if (hasTrades) orderBook.lastTrade else None
    aggregatedRef ! AggregatedOrderBookActor.Command.ApplyChanges(levelChanges, lastTrade, None, timestamp)
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
        aggregatedRef ! AggregatedOrderBookActor.Command.ApplyChanges(levelChanges, None, None, cancelEvent.timestamp)
        processEvents(List(cancelEvent))
      case _ =>
        log.warn(s"Error applying $event: order not found")
        addressActor ! OrderCancelFailed(id, error.OrderNotFound(id))
    }
  }

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, acceptedOrder: AcceptedOrder): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    process(
      eventWithMeta.timestamp,
      orderBook.add(acceptedOrder, eventWithMeta.timestamp, getMakerTakerFeeByOffset(eventWithMeta.offset), actualRule.tickSize)
    )
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

  case class Settings(aggregated: AggregatedOrderBookActor.Settings)

  def props(settings: Settings,
            parent: classic.ActorRef,
            addressActor: classic.ActorRef,
            snapshotStore: classic.ActorRef,
            assetPair: AssetPair,
            amountDecimals: Int,
            priceDecimals: Int,
            time: Time,
            matchingRules: NonEmptyList[DenormalizedMatchingRule],
            updateCurrentMatchingRules: DenormalizedMatchingRule => Unit,
            normalizeMatchingRule: DenormalizedMatchingRule => MatchingRule,
            getMakerTakerFeeByOffset: Long => (AcceptedOrder, LimitOrder) => (Long, Long),
            restrictions: Option[OrderRestrictionsSettings])(implicit ec: ExecutionContext): classic.Props =
    classic.Props(
      new OrderBookActor(
        settings,
        parent,
        addressActor,
        snapshotStore,
        assetPair,
        amountDecimals,
        priceDecimals,
        time,
        matchingRules,
        updateCurrentMatchingRules,
        normalizeMatchingRule,
        getMakerTakerFeeByOffset,
        restrictions
      )
    )

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[LevelAgg],
      bestAsk: Option[LevelAgg]
  )

  object MarketStatus {
    def apply(ob: OrderBook): MarketStatus = MarketStatus(ob.lastTrade, ob.bestBid, ob.bestAsk)
  }

  case class Snapshot(eventNr: Option[Long], orderBook: OrderBookSnapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdateCompleted(assetPair: AssetPair, currentOffset: Option[Long])
  case object SendWsUpdates
}
