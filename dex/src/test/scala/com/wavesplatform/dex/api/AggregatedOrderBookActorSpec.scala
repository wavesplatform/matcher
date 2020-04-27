package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicReference

import akka.actor.Props
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.model.HttpResponse
import akka.testkit.TestProbe
import cats.data.NonEmptyList
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.actors.OrderBookAskAdapter
import com.wavesplatform.dex.api.http.TestParsers
import com.wavesplatform.dex.db.OrderBookSnapshotDB
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.market.AggregatedOrderBookActor.{Message, Query, WsCommand}
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.market._
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.time.SystemTime
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._

class AggregatedOrderBookActorSpec
    extends AnyFreeSpec
    with Matchers
    with SystemTime
    with ScalaCheckPropertyChecks
    with NoShrink
    with MatcherSpecLike
    with TestParsers
    with OrderBookGen
    with DiffMatcherWithImplicits
    with Eventually {

  import system.dispatcher

  private val pair = AssetPair(IssuedAsset(ByteStr("issued".getBytes(StandardCharsets.UTF_8))), Waves)

  private val maxLevelsInOrderBook = 6
  private val maxOrdersInLevel     = 2

  // TODO migrate to long ranges in 2.13
  private val askPricesMin = 1000L * Order.PriceConstant
  private val askPricesMax = 2000L * Order.PriceConstant
  private val askPricesGen = Gen.choose(askPricesMin, askPricesMax)

  private val bidPricesMin = 1L * Order.PriceConstant
  private val bidPricesMax = 999L * Order.PriceConstant
  private val bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)

  private val pricesGen = Gen.choose(bidPricesMin, askPricesMax)

  private val orderBookGen =
    flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen).map(Function.tupled(mkOrderBook))

  private val askGen = orderGen(pricesGen, OrderType.SELL)
  private val bidGen = orderGen(pricesGen, OrderType.BUY)

  private def ordersGen(maxOrdersNumber: Int): Gen[List[AcceptedOrder]] =
    for {
      orderSides <- Gen.resize(maxOrdersNumber, Gen.listOf(orderSideGen))
      orders <- Gen.sequence {
        orderSides.map { side =>
          val orderGen = if (side == OrderType.SELL) askGen else bidGen
          Gen.oneOf(limitOrderGen(orderGen), marketOrderGen(orderGen))
        }
      }
    } yield orders.asScala.toList

  "AggregatedOrderBookActor" - {
    "properties" - {
      "aggregate(updatedOrderBook) == updatedAggregatedOrderBook" in forAll(orderBookGen, ordersGen(10)) { (initOb, orders) =>
        val obsdb = OrderBookSnapshotDB.inMem
        obsdb.update(pair, 0, Some(initOb.snapshot))

        val owner = TestProbe()
        val orderBookRef = system.actorOf(
          Props(new OrderBookActor(
            OrderBookActor.Settings(AggregatedOrderBookActor.Settings(100.millis)),
            owner.ref,
            TestProbe().ref,
            system.actorOf(OrderBookSnapshotStoreActor.props(obsdb)),
            pair,
            8,
            8,
            time,
            NonEmptyList.one(DenormalizedMatchingRule(0L, DenormalizedMatchingRule.DefaultTickSize)),
            _ => (),
            raw => MatchingRule(raw.startOffset, (raw.tickSize * BigDecimal(10).pow(8)).toLongExact),
            _ => (t, m) => m.matcherFee -> t.matcherFee
          )))
        owner.expectMsgType[OrderBookActor.OrderBookRecovered]

        orders.foreach(orderBookRef ! _)

        orderBookRef ! MatcherActor.SaveSnapshot(100L)
        owner.expectMsgType[OrderBookActor.OrderBookSnapshotUpdateCompleted]

        val orderBookAskAdapter = new OrderBookAskAdapter(new AtomicReference(Map(pair -> Right(orderBookRef))), 5.seconds)

        val updatedOrderBook = eventually {
          val ob = obsdb.get(pair)
          ob shouldNot be(empty)
          ob.get._2
        }

        val actual = Await.result(orderBookAskAdapter.getAggregatedSnapshot(pair), 5.seconds).explicitGet().get

        val expected = OrderBookAggregatedSnapshot(
          asks = sum(updatedOrderBook.asks),
          bids = sum(updatedOrderBook.bids)
        )

        actual should matchTo(expected)
      }
    }

    "apply" - {
      "should init with provided order book" in forAll(orderBookGen) { orderBook =>
        val ref      = mk(orderBook)
        val actual   = orderBookFrom { get[HttpResponse](ref)(Query.GetHttpView(MatcherModel.Normalized, 10, _)) }
        val expected = orderBookResultFrom(orderBook)
        actual.copy(timestamp = 0L) should matchTo(expected)
      }
    }

    "should return an updated" - {
      "market status after update" - {
        "once" in {
          val ref       = mk(OrderBook.empty)
          val lastTrade = LastTrade(500L, 10L, OrderType.BUY)

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty.copy(
              asks = Map(2000L -> 99L)
            ),
            lastTrade = Some(lastTrade),
            ts = 0L
          )

          val actual = get(ref)(Query.GetMarketStatus)
          val expected = MarketStatus(
            lastTrade = Some(lastTrade),
            bestAsk = Some(LevelAgg(99L, 2000L)),
            bestBid = None
          )

          actual should matchTo(expected)
        }

        "multiple times" in {
          val ref       = mk(OrderBook.empty)
          val lastTrade = LastTrade(500L, 10L, OrderType.BUY)

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty.copy(
              asks = Map(2000L -> 99L)
            ),
            lastTrade = Some(LastTrade(499L, 1L, OrderType.BUY)),
            ts = 0L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty,
            lastTrade = Some(lastTrade),
            ts = 1L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty.copy(
              bids = Map(1000L -> 84L)
            ),
            lastTrade = None,
            ts = 2L
          )

          val actual = get(ref)(Query.GetMarketStatus)
          val expected = MarketStatus(
            lastTrade = Some(lastTrade),
            bestAsk = Some(LevelAgg(99L, 2000L)),
            bestBid = Some(LevelAgg(84L, 1000L))
          )

          actual should matchTo(expected)
        }
      }

      "aggregated snapshot after update" - {
        "once" in {
          val ref = mk(OrderBook.empty)
          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2100L -> 1L, 2000L  -> 99L),
              bids = Map(999L  -> 30L, 1000L -> 50L)
            ),
            lastTrade = Some(LastTrade(500L, 10L, OrderType.BUY)),
            ts = 0L
          )

          val actual = get(ref)(Query.GetAggregatedSnapshot)
          val expected = OrderBookAggregatedSnapshot(
            asks = List(LevelAgg(99L, 2000L), LevelAgg(1L, 2100L)),
            bids = List(LevelAgg(50L, 1000L), LevelAgg(30L, 999L))
          )

          actual should matchTo(expected)
        }

        "multiple times" in {
          val ref = mk(OrderBook.empty)

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2100L -> 1L),
              bids = Map.empty
            ),
            lastTrade = None,
            ts = 0L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2000L -> 99L),
              bids = Map.empty
            ),
            lastTrade = None,
            ts = 1L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map.empty,
              bids = Map(999L -> 30L, 1000L -> 50L)
            ),
            lastTrade = None,
            ts = 2L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty,
            lastTrade = Some(LastTrade(500L, 10L, OrderType.BUY)),
            ts = 3L
          )

          val actual = get(ref)(Query.GetAggregatedSnapshot)
          val expected = OrderBookAggregatedSnapshot(
            asks = List(LevelAgg(99L, 2000L), LevelAgg(1L, 2100L)),
            bids = List(LevelAgg(50L, 1000L), LevelAgg(30L, 999L))
          )

          actual should matchTo(expected)
        }
      }

      "http response after update" - {
        "once" in {
          val ref = mk(OrderBook.empty)
          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2100L -> 1L, 2000L  -> 99L),
              bids = Map(999L  -> 30L, 1000L -> 50L)
            ),
            lastTrade = Some(LastTrade(500L, 10L, OrderType.BUY)),
            ts = 1L
          )

          val actual = orderBookFrom(get[HttpResponse](ref)(Query.GetHttpView(MatcherModel.Normalized, 1, _)))
          val expected = OrderBookResult(
            pair = pair,
            assetPairDecimals = None,
            asks = List(LevelAgg(99L, 2000L)),
            bids = List(LevelAgg(50L, 1000L)),
            timestamp = 1L
          )

          actual should matchTo(expected)
        }

        "multiple times" in {
          val ref = mk(OrderBook.empty)

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2100L -> 1L),
              bids = Map.empty
            ),
            lastTrade = None,
            ts = 0L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map(2000L -> 99L),
              bids = Map.empty
            ),
            lastTrade = None,
            ts = 1L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts(
              asks = Map.empty,
              bids = Map(999L -> 30L, 1000L -> 50L)
            ),
            lastTrade = None,
            ts = 2L
          )

          ref ! WsCommand.ApplyChanges(
            levelChanges = LevelAmounts.empty,
            lastTrade = Some(LastTrade(500L, 10L, OrderType.BUY)),
            ts = 3L
          )

          val actual = orderBookFrom(get[HttpResponse](ref)(Query.GetHttpView(MatcherModel.Normalized, 2, _)))
          val expected = OrderBookResult(
            pair = pair,
            assetPairDecimals = None,
            asks = List(LevelAgg(99L, 2000L), LevelAgg(1L, 2100L)),
            bids = List(LevelAgg(50L, 1000L), LevelAgg(30L, 999L)),
            timestamp = 3L
          )

          actual should matchTo(expected)
        }
      }
    }
  }

  private def orderBookResultFrom(ob: OrderBook): OrderBookResult = OrderBookResult(
    pair = pair,
    asks = levelAggsFromSide(ob.asks),
    bids = levelAggsFromSide(ob.bids),
    assetPairDecimals = None,
    timestamp = 0L
  )

  private def levelAggsFromSide(side: Side): List[LevelAgg] =
    AggregatedOrderBookActor.aggregateByPrice(side).map(AggregatedOrderBookActor.toLevelAgg).toList.sortBy(_.price)(side.ordering)

  private def sum(side: OrderBookSideSnapshot): List[LevelAgg] =
    side.map {
      case (price, level) => LevelAgg(amount = level.map(_.amount).sum, price = price)
    }.toList

  private def mk(ob: OrderBook): ActorRef[Message] = system.spawn(
    AggregatedOrderBookActor(
      AggregatedOrderBookActor.Settings(100.millis),
      pair,
      8,
      8,
      AggregatedOrderBookActor.State.fromOrderBook(ob)
    ),
    s"aggregated-${ThreadLocalRandom.current().nextInt()}"
  )

  private def get[R](ref: ActorRef[Message])(mkMessage: ActorRef[R] => Message): R =
    Await.result(ref.ask[R](mkMessage)(5.seconds, system.scheduler.toTyped), 5.seconds)

  override protected def actorSystemName: String = "AggregatedOrderBookSpec"
}
