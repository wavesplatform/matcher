package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BlockchainCacheSpecification extends WordSpecLike with Matchers {

  private class BlockchainCacheTest(loader: String => Future[String], expiration: Option[Duration], invalidationPredicate: String => Boolean)
      extends BlockchainCache[String, String](loader, expiration, invalidationPredicate)

  private def createCache(loader: String => Future[String],
                          expiration: Option[Duration] = None,
                          invalidationPredicate: String => Boolean = _ => false): BlockchainCacheTest = {
    new BlockchainCacheTest(loader, expiration, invalidationPredicate)
  }

  "BlockchainCache" should {

    "not keep failed futures" in {

      val keyAccessMap = collection.mutable.Map.empty[String, Int]
      val gRPCError    = new RuntimeException("gRPC Error occurred")
      val badKey       = "gRPC Error"

      val cache =
        createCache(
          key => {
            keyAccessMap += key -> (keyAccessMap.getOrElse(key, 0) + 1)
            if (key == badKey) Future.failed(gRPCError) else Future.successful(s"value = $key")
          }
        )

      (1 to 5) foreach { _ =>
        cache get "goodKey"
        cache get badKey
        Thread.sleep(100)
      }

      keyAccessMap shouldBe
        Map(
          "goodKey" -> 1,
          badKey    -> 5
        )
    }

    "not keep values according to the predicate" in {

      val keyAccessMap = collection.mutable.Map.empty[String, Int]

      val cache = createCache(
        key => {
          keyAccessMap += key -> (keyAccessMap.getOrElse(key, 0) + 1)
          Future.successful { Thread.sleep(50); key }
        },
        invalidationPredicate = result => result startsWith "2"
      )

      (1 to 5) foreach { _ =>
        cache get "111"
        cache get "222"
        Thread.sleep(100)
      }

      keyAccessMap shouldBe
        Map(
          "111" -> 1,
          "222" -> 5
        )
    }
  }
}
