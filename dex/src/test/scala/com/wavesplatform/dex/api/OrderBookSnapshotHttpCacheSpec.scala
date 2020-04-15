//package com.wavesplatform.dex.api
//
//import java.nio.charset.StandardCharsets
//
//import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
//import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
//import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
//import com.wavesplatform.dex.domain.bytes.ByteStr
//import com.wavesplatform.dex.model._
//import com.wavesplatform.dex.time.SystemTime
//import org.scalatest.freespec.AnyFreeSpec
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.prop.TableDrivenPropertyChecks
//
//import scala.concurrent.duration._
//
//// TODO Rename
//class OrderBookSnapshotHttpCacheSpec extends AnyFreeSpec with Matchers with SystemTime with TableDrivenPropertyChecks {
//
//  private val defaultAssetPair                            = AssetPair(Waves, IssuedAsset(ByteStr("asset".getBytes("utf-8"))))
//  private def getAssetDecimals(asset: Asset): Option[Int] = Some(8)
//
//  "OrderBookSnapshotHttpCache" - {
//
//    "happy path" in using(createDefaultCache) { cache =>
//      def get: HttpResponse = cache.get(defaultAssetPair, Some(1), MatcherModel.Denormalized)
//
//      val a = get
//      val b = get
//
//      a shouldBe b
//    }
//
//    "should return update an updated" - {
//      "market status after update" in using(createDefaultCache) { cache =>
//        def get: HttpResponse = cache.get(defaultAssetPair, Some(1))
//
//        val a = get
//        Thread.sleep(30)
//        val b = get
//
//        a shouldBe b
//      }
//
//      "aggregated snapshot after update" in using(createDefaultCache) { cache =>
//        def get: HttpResponse = cache.get(defaultAssetPair, Some(1))
//
//        val a = get
//        Thread.sleep(30)
//        val b = get
//
//        a shouldBe b
//      }
//
//      "http response after update" in using(createDefaultCache) { cache =>
//        def get: HttpResponse = cache.get(defaultAssetPair, Some(1))
//
//        val a = get
//        Thread.sleep(30)
//        val b = get
//
//        a shouldBe b
//      }
//    }
//
//
//  }
//
//  private def createDefaultCache =
//    new OrderBookSnapshotHttpCache(OrderBookSnapshotHttpCache.Settings(50.millis, List(3, 9), None), time, getAssetDecimals, _ => None)
//
//  private def orderBookFrom(x: HttpResponse): OrderBookResult = JsonSerializer.deserialize[OrderBookResult](
//    x.entity
//      .asInstanceOf[HttpEntity.Strict]
//      .getData()
//      .decodeString(StandardCharsets.UTF_8)
//  )
//
//  private def using[T <: AutoCloseable](create: => T)(f: T => Unit): Unit = {
//    val x = create
//    try {
//      f(x)
//    } finally {
//      x.close()
//    }
//  }
//}
