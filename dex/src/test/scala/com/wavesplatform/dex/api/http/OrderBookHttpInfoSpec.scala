package com.wavesplatform.dex.api.http

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{Actor, Props}
import cats.instances.future._
import cats.syntax.applicative._
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor
import com.wavesplatform.dex.actors.{MatcherSpecLike, OrderBookAskAdapter}
import com.wavesplatform.dex.api.http.OrderBookHttpInfoSpec.FakeOrderBookActor
import com.wavesplatform.dex.api.http.entities.HttpV0OrderBook
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts, MatcherModel, OrderBook}
import com.wavesplatform.dex.settings.DenormalizedMatchingRule
import com.wavesplatform.dex.time.{SystemTime, Time}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class OrderBookHttpInfoSpec extends AnyFreeSpec with Matchers with SystemTime with TableDrivenPropertyChecks with MatcherSpecLike {

  private val pair = AssetPair(IssuedAsset(ByteStr("issued".getBytes(StandardCharsets.UTF_8))), Waves)

  "OrderBookHttpInfo" - {
    "getHttpView" - {
      "should return the nearest depth cache" - {
        // Two levels: one is aggregated and one is not

        val aggOrderBookRef = system.actorOf(Props(new FakeOrderBookActor(pair)))
        val askAdapter = new OrderBookAskAdapter(new AtomicReference(Map(pair -> Right(aggOrderBookRef))), timeout)
        val orderBookHttpInfo = new OrderBookHttpInfo(OrderBookHttpInfo.Settings(List(3, 9), None), askAdapter, time, _ => 8.pure[FutureResult])
        def get(depth: Option[Int]): HttpV0OrderBook =
          HttpV0OrderBook.fromHttpResponse(orderBookHttpInfo.getHttpView(pair, MatcherModel.Normalized, depth).futureValue)

        val middlePrice = 1000L
        val now = time.getTimestamp()

        (1 to 10).foreach { i =>
          aggOrderBookRef ! AggregatedOrderBookActor.Command.ApplyChanges(
            LevelAmounts(
              asks = Map((middlePrice + i) -> i * 2L),
              bids = Map((middlePrice - i) -> i * 3L)
            ),
            lastTrade = Some(LastTrade(middlePrice, 5, OrderType.SELL)),
            tickSize = None,
            ts = now + i
          )
        }

        "None -> 9" in {
          val ob = get(None)
          ob.bids.size shouldBe 9
        }

        Seq(
          0 -> 3,
          1 -> 3,
          3 -> 3,
          5 -> 9,
          10 -> 9
        ).foreach {
          case (depth, expectedSize) =>
            s"$depth -> $expectedSize" in {
              val ob = get(Some(depth))
              ob.bids.size shouldBe expectedSize
            }
        }
      }
    }

    "Settings.nearestBigger" - {

      def mkSettings(depthRanges: List[Int], defaultDepth: Option[Int]): OrderBookHttpInfo.Settings =
        OrderBookHttpInfo.Settings(depthRanges, defaultDepth)

      "1 and None" in {
        val settings = mkSettings(List(1), None)
        forAll(
          Table(
            ("arg", "expected"),
            (None, 1),
            (Some(0), 1),
            (Some(1), 1),
            (Some(5), 1)
          )
        ) { (arg, expected) =>
          settings.nearestBigger(arg) shouldBe expected
        }
      }

      "1, 3, 7, 9 and None" in {
        val settings = mkSettings(List(1, 3, 7, 9), None)
        forAll(
          Table(
            ("arg", "expected"),
            (None, 9),
            (Some(0), 1),
            (Some(1), 1),
            (Some(5), 7),
            (Some(7), 7),
            (Some(9), 9),
            (Some(100), 9)
          )
        ) { (arg, expected) =>
          settings.nearestBigger(arg) shouldBe expected
        }
      }

      "1, 3, 7, 9 and Some(3)" in {
        val settings = mkSettings(List(1, 3, 7, 9), Some(3))
        forAll(
          Table(
            ("arg", "expected"),
            (None, 3),
            (Some(0), 1),
            (Some(1), 1),
            (Some(5), 7),
            (Some(7), 7),
            (Some(9), 9),
            (Some(100), 9)
          )
        ) { (arg, expected) =>
          settings.nearestBigger(arg) shouldBe expected
        }
      }
    }
  }

}

object OrderBookHttpInfoSpec {

  private class FakeOrderBookActor(pair: AssetPair) extends Actor {

    private val aggOrderBookRef = context.spawn(
      AggregatedOrderBookActor(
        AggregatedOrderBookActor.Settings(100.millis),
        pair,
        8,
        8,
        None,
        DenormalizedMatchingRule.DefaultTickSize.toDouble,
        Time.zero,
        AggregatedOrderBookActor.State.fromOrderBook(OrderBook.empty)
      ),
      "aggregated"
    )

    override def receive: Receive = {
      case x: AggregatedOrderBookActor.InputMessage => aggOrderBookRef ! x
    }

  }

}
