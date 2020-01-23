package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.time.NTPTime
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._

class OrderBookSnapshotHttpCacheSpec extends AnyFreeSpec with Matchers with NTPTime with TableDrivenPropertyChecks {

  private val defaultAssetPair                            = AssetPair(Waves, IssuedAsset(ByteStr("asset".getBytes("utf-8"))))
  private def getAssetDecimals(asset: Asset): Option[Int] = Some(8)

  "OrderBookSnapshotHttpCache" - {

    "should cache" in using(createDefaultCache) { cache =>
      def get: HttpResponse = cache.get(defaultAssetPair, Some(1), MatcherModel.Denormalized)

      val a = get
      val b = get

      a shouldBe b
    }

    "should not drop the cache if the timeout after an access was not reached" in using(createDefaultCache) { cache =>
      def get: HttpResponse = cache.get(defaultAssetPair, Some(1))

      val a = get
      Thread.sleep(30)
      val b = get

      a shouldBe b
    }

    "should drop the cache after timeout" in using(createDefaultCache) { cache =>
      def get: HttpResponse = cache.get(defaultAssetPair, Some(1))

      val a = get
      Thread.sleep(70)
      val b = get

      a shouldNot be(b)
    }

    "should return the nearest depth cache" - {
      // Two levels: one is aggregated and one is not
      using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, List(3, 9), None),
          ntpTime,
          getAssetDecimals,
          _ =>
            Some(
              OrderBook.AggregatedSnapshot(
                Seq.tabulate(15)(i => LevelAgg(200 - i * 10, 1000 - 10 * i)),
                Seq.tabulate(15)(i => LevelAgg(200 - i * 10, 1000 - 10 * i)),
              )
          )
        )
      } { cache =>
        "None -> 9" in {
          val ob = orderBookFrom(cache.get(defaultAssetPair, None))
          ob.bids.size shouldBe 9
        }

        Seq(
          0  -> 3,
          1  -> 3,
          3  -> 3,
          5  -> 9,
          10 -> 9
        ).foreach {
          case (depth, expectedSize) =>
            s"$depth -> $expectedSize" in {
              val ob = orderBookFrom(cache.get(defaultAssetPair, Some(depth)))
              ob.bids.size shouldBe expectedSize
            }
        }
      }
    }

    "should clear all depth caches after invalidate" in {
      val depths = List(1, 3, 7, 9)
      using {
        new OrderBookSnapshotHttpCache(OrderBookSnapshotHttpCache.Settings(1.minute, depths, None), ntpTime, getAssetDecimals, _ => None)
      } { cache =>
        val prev = depths.map(x => x -> orderBookFrom(cache.get(defaultAssetPair, Some(x)))).toMap

        Thread.sleep(100)
        cache.invalidate(defaultAssetPair)

        depths.foreach { depth =>
          withClue(s"cache for depth=$depth was invalidated") {
            val curr = orderBookFrom(cache.get(defaultAssetPair, Some(depth)))
            curr.timestamp shouldNot be(prev(depth).timestamp)
          }
        }
      }
    }

    "Settings.nearestBigger" - {
      def mkSettings(depthRanges: List[Int], defaultDepth: Option[Int]) = OrderBookSnapshotHttpCache.Settings(50.millis, depthRanges, defaultDepth)

      "1 and None" in {
        val settings = mkSettings(List(1), None)
        forAll(
          Table(
            ("arg", "expected"),
            (None, 1),
            (Some(0), 1),
            (Some(1), 1),
            (Some(5), 1)
          )) { (arg, expected) =>
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
          )) { (arg, expected) =>
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
          )) { (arg, expected) =>
          settings.nearestBigger(arg) shouldBe expected
        }
      }
    }
  }

  private def createDefaultCache =
    new OrderBookSnapshotHttpCache(OrderBookSnapshotHttpCache.Settings(50.millis, List(3, 9), None), ntpTime, getAssetDecimals, _ => None)

  private def orderBookFrom(x: HttpResponse): OrderBookResult = JsonSerializer.deserialize[OrderBookResult](
    x.entity
      .asInstanceOf[HttpEntity.Strict]
      .getData()
      .decodeString(StandardCharsets.UTF_8)
  )

  private def using[T <: AutoCloseable](create: => T)(f: T => Unit): Unit = {
    val x = create
    try {
      f(x)
    } finally {
      x.close()
    }
  }
}
