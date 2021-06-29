package com.wavesplatform.dex.actors

import akka.actor.{Actor, ActorRef, Props, Stash, SupervisorStrategy, Terminated}
import cats.implicits.catsSyntaxEitherId
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.{OrderBookRecovered, OrderBookSnapshotUpdateCompleted}
import com.wavesplatform.dex.actors.orderbook.{AggregatedOrderBookActor, OrderBookActor}
import com.wavesplatform.dex.api.http.entities.OrderBookUnavailable
import com.wavesplatform.dex.app.{forceStopApplication, StartingMatcherError}
import com.wavesplatform.dex.db.{AssetPairsDb, AssetsCache}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.effect.Implicits.FutureCompanionOps
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.{Offset => EventOffset}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.settings.MatcherSettings
import kamon.Kamon
import scorex.utils._

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.util.{Failure, Success}

class OrderBookDirectoryActor(
  settings: MatcherSettings,
  assetPairsDb: AssetPairsDb[Future],
  recoveryCompletedWithEventNr: Either[String, Long] => Unit,
  orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
  orderBookActorProps: (AssetPair, ActorRef) => Props,
  assetsCache: AssetsCache,
  validateAssetPair: AssetPair => Either[MatcherError, AssetPair]
) extends Actor
    with Stash
    with ScorexLogging {

  import OrderBookDirectoryActor._
  import context.dispatcher

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private val currentOffsetGauge =
    Kamon.gauge("matcher.queue.current").withTag("node", settings.id)

  private val snapshotOffsetGauge =
    Kamon.gauge("matcher.snapshots.oldest").withTag("node", settings.id)

  private var tradedPairs: Map[AssetPair, MarketData] = Map.empty
  private var lastProcessedNr: Long = -1L

  private var snapshotsState = SnapshotsState.empty

  override val receive: Receive = {
    case Start(assetPairs) =>
      val (errors, validAssetPairs) = assetPairs.partitionMap { assetPair =>
        validateAssetPair(assetPair) match {
          case Left(e) => s"$assetPair: ${e.message.text}".asLeft
          case Right(x) => x.asRight
        }
      }

      if (errors.nonEmpty) log.warn(s"Invalid asset pairs:\n${errors.mkString("\n")}")

      val nextState =
        if (validAssetPairs.isEmpty) {
          log.info("Recovery completed!")
          recoveryCompletedWithEventNr(-1L asRight)
          working
        } else {
          log.info(s"Recovery completed, waiting ${validAssetPairs.size} order books to restore")
          validAssetPairs.foreach(createOrderBook)
          collectOrderBooks(validAssetPairs.size, None, -1L, Map.empty)
        }

      unstashAll()
      context.become(nextState)

    case _ => stash()
  }

  private def orderBook(pair: AssetPair): Option[Either[Unit, ActorRef]] = Option(orderBooks.get()).flatMap(_.get(pair))

  private def getAssetName(asset: Asset, desc: Option[BriefAssetDescription]): String =
    asset match {
      case Waves => Asset.WavesName
      case _ => desc.fold("Unknown")(_.name)
    }

  private def getAssetInfo(asset: Asset, desc: Option[BriefAssetDescription]): Option[AssetInfo] =
    asset.fold(Option(8))(_ => desc.map(_.decimals)).map(AssetInfo)

  private def getAssetDesc(asset: Asset): Option[BriefAssetDescription] =
    asset.fold[Option[BriefAssetDescription]](None)(assetsCache.cached.get)

  private def createMarketData(pair: AssetPair): MarketData = {
    val amountAssetDescription = getAssetDesc(pair.amountAsset)
    val priceAssetDescription = getAssetDesc(pair.priceAsset)

    MarketData(
      pair,
      getAssetName(pair.amountAsset, amountAssetDescription),
      getAssetName(pair.priceAsset, priceAssetDescription),
      System.currentTimeMillis(),
      getAssetInfo(pair.amountAsset, amountAssetDescription),
      getAssetInfo(pair.priceAsset, priceAssetDescription)
    )
  }

  private def createOrderBook(pair: AssetPair): ActorRef = {
    log.info(s"Creating order book for $pair")
    val orderBook = context.watch(context.actorOf(orderBookActorProps(pair, self), OrderBookActor.name(pair)))
    orderBooks.updateAndGet(_ + (pair -> Right(orderBook)))
    tradedPairs += pair -> createMarketData(pair)
    orderBook
  }

  /**
   * @param f (sender, orderBook)
   */
  private def runFor(
    assetPair: AssetPair,
    maybeOffset: Option[EventOffset] = None,
    autoCreate: Boolean = true
  )(f: (ActorRef, ActorRef) => Unit): Unit = {
    val s = sender()
    orderBook(assetPair) match {
      case Some(Right(ob)) => f(s, ob)
      case Some(Left(_)) => s ! OrderBookUnavailable(error.OrderBookBroken(assetPair))
      case None =>
        if (context.child(OrderBookActor.name(assetPair)).nonEmpty) {
          log.error(s"OrderBook for $assetPair is stopped, but it is not observed in orderBook")
          s ! OrderBookUnavailable(error.OrderBookUnexpectedState(assetPair))
        } else if (autoCreate) {
          val ob = createOrderBook(assetPair)
          assetPairsDb
            .add(assetPair)
            .onComplete {
              case Failure(e) => log.error(s"Can't save $assetPair", e)
              case _ =>
            }
          f(s, ob)
          maybeOffset.foreach(createSnapshotFor(ob, _))
        } else {
          log.warn(s"OrderBook for $assetPair is stopped and autoCreate is $autoCreate, respond to client with OrderBookUnavailable")
          s ! OrderBookUnavailable(error.OrderBookStopped(assetPair))
        }
    }
  }

  private def createSnapshotFor(orderbook: ActorRef, offset: EventOffset): Unit =
    orderbook ! SaveSnapshot(offset)

  private def createSnapshotFor(offset: ValidatedCommandWithMeta.Offset): Unit =
    snapshotsState.requiredSnapshot(offset).foreach { case (assetPair, updatedSnapshotState) =>
      orderBook(assetPair) match {
        case Some(Right(actorRef)) =>
          log.info(
            s"The $assetPair order book should do a snapshot, the current offset is $offset. The next snapshot candidate: ${updatedSnapshotState.nearestSnapshotOffset}"
          )
          actorRef ! SaveSnapshot(offset)

        case Some(Left(_)) => log.warn(s"Can't create a snapshot for $assetPair: the order book is down, ignoring it in the snapshot's rotation.")
        case None => log.warn(s"Can't create a snapshot for $assetPair: the order book hasn't yet started or was removed.")
      }
      snapshotsState = updatedSnapshotState
      snapshotsState.nearestSnapshotOffset.foreach { snapshotOffset =>
        snapshotOffsetGauge.update(snapshotOffset._2.toDouble)
      }
    }

  private def working: Receive = {

    case GetMarkets => sender() ! tradedPairs.values.toList
    case GetSnapshotOffsets => sender() ! SnapshotOffsetsResponse(snapshotsState.snapshotOffsets)

    // DEX-1192 docs/places-and-cancels.md
    case request: ValidatedCommandWithMeta =>
      val currentLastProcessedNr = math.max(request.offset, lastProcessedNr)
      request.command match {
        case ValidatedCommand.DeleteOrderBook(assetPair, _) =>
          // autoCreate = false for case, when multiple OrderBookDeleted(A1-A2) events happen one after another
          runFor(request.command.assetPair, Some(currentLastProcessedNr), autoCreate = false) { (sender, ref) =>
            ref.tell(request, sender)
            orderBooks.getAndUpdate(_.filterNot(_._2.exists(_ == ref)))
            snapshotsState = snapshotsState.without(assetPair)
            tradedPairs -= assetPair
            assetPairsDb
              .remove(assetPair)
              .onComplete {
                case Failure(e) => log.error(s"Can't remove $assetPair", e)
                case _ =>
              }
          }

        case _ => runFor(request.command.assetPair, Some(currentLastProcessedNr))((sender, orderBook) => orderBook.tell(request, sender))
      }
      lastProcessedNr = currentLastProcessedNr
      currentOffsetGauge.update(lastProcessedNr.toDouble)
      createSnapshotFor(lastProcessedNr)

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)

    case Terminated(ref) =>
      val orderBookActorName = ref.path.name
      val xs = orderBookActorName.split('-')

      val pair =
        (if (xs.length == 2) AssetPair.createAssetPair(xs.head, xs(1))
         else Failure(new IllegalArgumentException(s"Can't extract a pair from the order book name: '$orderBookActorName'"))).toOption

      pair.foreach { p =>
        orderBooks.getAndUpdate { obs =>
          obs.get(p) match {
            case None => obs
            case Some(_) => obs.updated(p, Left(()))
          }
        }
      }

      if (pair.fold(true)(orderBooks.get.contains)) log.error(s"$ref is terminated")
      else log.info(s"$ref is terminated")

    case OrderBookRecovered(assetPair, eventNr) =>
      snapshotsState = snapshotsState.updated(assetPair, eventNr, lastProcessedNr, settings.snapshotsInterval)

    case OrderBookSnapshotUpdateCompleted(assetPair, currentOffset) =>
      snapshotsState = snapshotsState.updated(assetPair, currentOffset, lastProcessedNr, settings.snapshotsInterval)

    case PingAll(xs) =>
      val workers = xs.flatMap(pair => context.child(pair.key))
      val s = sender()
      context.actorOf(WatchDistributedCompletionActor.props(workers, s, Ping, Pong, settings.processConsumedTimeout))

    case AggregatedOrderBookEnvelope(pair, message) => runFor(pair) { (sender, ref) =>
        ref.tell(message, sender)
      }

    case ForceSaveSnapshots => context.children.foreach(_ ! SaveSnapshot(lastProcessedNr))
  }

  private def collectOrderBooks(
    restOrderBooksNumber: Long,
    oldestEventNr: Option[Long],
    newestEventNr: Long,
    currentOffsets: Map[AssetPair, Option[EventOffset]]
  ): Receive = {
    case OrderBookRecovered(assetPair, snapshotEventNr) =>
      val updatedRestOrderBooksNumber = restOrderBooksNumber - 1

      val updatedOldestSnapshotOffset = (oldestEventNr, snapshotEventNr) match {
        case (Some(oldestNr), Some(orderBookNr)) => Some(math.min(oldestNr, orderBookNr))
        case (oldestNr, orderBookNr) => oldestNr.orElse(orderBookNr)
      }

      val updatedNewestEventNr = math.max(newestEventNr, snapshotEventNr.getOrElse(-1L))
      val updatedCurrentOffsets = currentOffsets.updated(assetPair, snapshotEventNr)

      if (updatedRestOrderBooksNumber > 0)
        context.become(collectOrderBooks(updatedRestOrderBooksNumber, updatedOldestSnapshotOffset, updatedNewestEventNr, updatedCurrentOffsets))
      else becomeWorking(updatedOldestSnapshotOffset, updatedNewestEventNr, updatedCurrentOffsets)

    case Terminated(ref) =>
      log.error(s"$ref is terminated during start, recovery failed")
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr(s"$ref is terminated".asLeft)

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr("Received Shutdown command".asLeft)

    case _ => stash()
  }

  private def becomeWorking(
    oldestSnapshotOffset: Option[EventOffset],
    newestSnapshotOffset: EventOffset,
    currentOffsets: Map[AssetPair, Option[EventOffset]]
  ): Unit = {
    context.become(working)

    val oldestOffset = oldestSnapshotOffset.getOrElse(-1L)

    val safestStartOffset = math.max(
      -1L,
      settings.limitEventsDuringRecovery.fold(oldestOffset) { limitEventsDuringRecovery =>
        math.max(oldestOffset, newestSnapshotOffset - limitEventsDuringRecovery)
      }
    )

    snapshotsState = SnapshotsState(
      currentOffsets = currentOffsets,
      lastProcessedOffset = newestSnapshotOffset,
      interval = settings.snapshotsInterval
    )

    log.info(
      s"All snapshots are loaded, oldestSnapshotOffset: $oldestSnapshotOffset, newestSnapshotOffset: $newestSnapshotOffset, " +
      s"safestStartOffset: $safestStartOffset, newestSnapshotOffset: $newestSnapshotOffset"
    )
    log.trace(s"Expecting next snapshots at:\n${snapshotsState.nearestSnapshotOffsets.map { case (p, x) => s"$p -> $x" }.mkString("\n")}")

    unstashAll()
    recoveryCompletedWithEventNr(safestStartOffset.asRight)
  }

  // Init

  val assetPairsInit = for {
    assetPairs <- assetPairsDb.all()
    // We need to do this, because assets must be cached before order books created
    _ <- Future.inSeries(assetPairs.flatMap(_.assets))(assetsCache.get)
  } yield assetPairs

  assetPairsInit.onComplete {
    case Success(xs) => self ! Start(xs)
    case Failure(e) =>
      log.error("Can't receive asset pairs", e)
      forceStopApplication(StartingMatcherError)
  }

}

object OrderBookDirectoryActor {

  def name: String = "matcher"

  def props(
    matcherSettings: MatcherSettings,
    assetPairsDB: AssetPairsDb[Future],
    recoveryCompletedWithEventNr: Either[String, Long] => Unit,
    orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
    orderBookProps: (AssetPair, ActorRef) => Props,
    assetsCache: AssetsCache,
    validateAssetPair: AssetPair => Either[MatcherError, AssetPair]
  ): Props = Props(
    new OrderBookDirectoryActor(
      matcherSettings,
      assetPairsDB,
      recoveryCompletedWithEventNr,
      orderBooks,
      orderBookProps,
      assetsCache,
      validateAssetPair
    )
  )

  private case class ShutdownStatus(initiated: Boolean, oldMessagesDeleted: Boolean, oldSnapshotsDeleted: Boolean, onComplete: () => Unit)

  private case class Start(knownAssetPairs: Set[AssetPair])

  case object ForceSaveSnapshots
  case class SaveSnapshot(globalEventNr: EventOffset)

  case class Snapshot(tradedPairsSet: Set[AssetPair])

  case class AggregatedOrderBookEnvelope(assetPair: AssetPair, message: AggregatedOrderBookActor.Message)

  case object GetMarkets

  case object GetSnapshotOffsets
  case class SnapshotOffsetsResponse(offsets: Map[AssetPair, Option[EventOffset]])

  case class MatcherRecovered(oldestEventNr: Long)

  case object Shutdown

  case class PingAll(pairs: Set[AssetPair])
  case object Ping
  case object Pong

  case class AssetInfo(decimals: Int)

  case class MarketData(
    pair: AssetPair,
    amountAssetName: String,
    priceAssetName: String,
    created: Long,
    amountAssetInfo: Option[AssetInfo],
    priceAssetInfo: Option[AssetInfo]
  )

  def compare(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Int =
    if (buffer1.isEmpty && buffer2.isEmpty) 0
    else if (buffer1.isEmpty) -1
    else if (buffer2.isEmpty) 1
    else ByteArray.compare(buffer1.get, buffer2.get)

}
