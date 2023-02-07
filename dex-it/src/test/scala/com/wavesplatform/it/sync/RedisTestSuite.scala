package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.protocol.{WsOrdersUpdate, WsServerMessage}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.HasRedis
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.redis.{RedisClient, RedisStream}
import com.wavesplatform.dex.settings.RedisSettings
import com.wavesplatform.it.{MatcherSuiteBase, WsSuiteBase}
import org.redisson.client.codec.StringCodec
import play.api.libs.json.Json

import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.util.Using

final class RedisTestSuite extends MatcherSuiteBase with HasRedis with WsSuiteBase {

  "Redis" - {

    "should write updates" - {

      "about single cancel order" in Using.resource(mkRedisClient()) { redisClient =>
        val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
        val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 5.usd)
        dex1.api.place(aliceOrder)
        dex1.api.cancelOrderById(aliceOrder, Some(alice.publicKey))

        checkMessages(stream, "singleCancelTest") { wou =>
          wou.orders.size shouldBe 1
          wou.orders.head.id shouldBe aliceOrder.id()
          wou.orders.head.status shouldBe OrderStatus.Cancelled.name
          wou.orders.head.amountAsset shouldBe Asset.Waves
          wou.orders.head.priceAsset shouldBe usd
        }
      }

      "about single match" in Using.resource(mkRedisClient()) { redisClient =>
        val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
        val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 5.usd)
        val counterOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 5.waves, 5.usd)
        dex1.api.place(aliceOrder)
        dex1.api.place(counterOrder)

        checkMessages(stream, "singleMatchTest") { wou =>
          wou.orders.size shouldBe 2
          wou.orders.map(_.id).toList.toSet shouldBe Set(aliceOrder.id(), counterOrder.id())
        }
      }

      "about market match" in Using.resource(mkRedisClient()) { redisClient =>
        val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
        val order1 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 1.waves, 3)
        val order2 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 2.waves, 3)
        val order3 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 3.waves, 4)

        List(order1, order2).foreach(dex1.api.place)
        placeAndAwaitAtDex(order3, Status.Filled, isMarketOrder = true)

        checkMessages(stream, "marketMatchTest") { wou =>
          wou.orders.size shouldBe 4
          wou.orders.map(_.id).toList.toSet shouldBe Set(order1.id(), order2.id(), order3.id())
        }
      }

      "when there is ws connection" in Using.resource(mkRedisClient()) { redisClient =>
        val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
        Using.resource(mkWsInternalConnection(dex1)) { wsInternal =>
          val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 5.usd)
          dex1.api.place(aliceOrder)
          dex1.api.cancelOrderById(aliceOrder, Some(alice.publicKey))

          val buffer = wsInternal.receiveAtLeastN[WsOrdersUpdate](1)
          buffer should have size 1

          val orderEvents = buffer.orderEvents
          orderEvents.keySet should matchTo(Set(aliceOrder.id()))

          checkMessages(stream, "withWsConnectionTest") { wou =>
            wou.orders.size shouldBe 1
            wou.orders.map(_.id).toList.toSet shouldBe Set(aliceOrder.id())
          }
        }
      }

      "after matcher restarts" in {
        Using.resource(mkRedisClient(redisPort = Some(getAddress(redisPort).getPort))) { redisClient =>

          val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 5.usd)
          dex1.api.place(aliceOrder)
          dex1.api.cancelOrderById(aliceOrder, Some(alice.publicKey))

          val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.SELL, 3.waves, 15.usd)
          dex1.api.place(aliceOrder2)
          dex1.api.cancelOrderById(aliceOrder2, Some(alice.publicKey))

          val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
          checkMessages(stream, "matcherDisconnectionTest") { wou: WsOrdersUpdate =>
            wou.orders.size shouldBe 2
            wou.orders.map(v => (v.id, v.status)).toList should contain((aliceOrder.id(), OrderStatus.Cancelled.name))
            wou.orders.map(v => (v.id, v.status)).toList should contain((aliceOrder2.id(), OrderStatus.Cancelled.name))
          }

          dex1.stopWithoutRemove()
          Thread.sleep(1500L)
          dex1.start()
          Thread.sleep(1500L)

          val aliceOrder3 = mkOrder(alice, wavesUsdPair, OrderType.SELL, 5.waves, 3.usd)
          dex1.api.place(aliceOrder3)
          dex1.api.cancelOrderById(aliceOrder3, Some(alice.publicKey))

          val aliceOrder4 = mkOrder(alice, wavesUsdPair, OrderType.SELL, 4.waves, 3.usd)
          dex1.api.place(aliceOrder4)
          dex1.api.cancelOrderById(aliceOrder4, Some(alice.publicKey))

          Thread.sleep(210L)

          val allUpdates = collectMessages(stream, "matcherDisconnectionTest")
          val aliceOrders = Seq(aliceOrder.id(), aliceOrder2.id(), aliceOrder3.id(), aliceOrder4.id())
          val aliceLastUpdates = allUpdates.collect {
            case wou if wou.orders.size == 2 && wou.orders.exists(v => aliceOrders.contains(v.id)) => wou
          }
          aliceLastUpdates.size shouldBe 2
          aliceLastUpdates.headOption.value.orders.map(_.id).toList.toSet shouldBe Set(aliceOrder.id(), aliceOrder2.id())
          aliceLastUpdates.headOption.value.orders.map(_.status).toList.toSet shouldBe Set(OrderStatus.Cancelled.name)
          aliceLastUpdates.lastOption.value.orders.map(_.id).toList.toSet shouldBe Set(aliceOrder3.id(), aliceOrder4.id())
          aliceLastUpdates.lastOption.value.orders.map(_.status).toList.toSet shouldBe Set(OrderStatus.Cancelled.name)
        }
      }

      "after redis disconnect" in {
        disconnectRedisFromNetwork()

        val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 5.usd)
        dex1.api.place(aliceOrder)
        dex1.api.cancelOrderById(aliceOrder, Some(alice.publicKey))

        val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.SELL, 3.waves, 15.usd)
        dex1.api.place(aliceOrder2)
        dex1.api.cancelOrderById(aliceOrder2, Some(alice.publicKey))

        Thread.sleep(5000L)

        connectRedisToNetwork()

        Thread.sleep(3000L)

        Using.resource(mkRedisClient(redisPort = Some(getAddress(redisPort).getPort))) { redisClient =>
          val stream = redisClient.getStream[String, String](redisStreamName, StringCodec.INSTANCE)
          checkMessages(stream, "redisDisconnectionTest") { wou: WsOrdersUpdate =>
            wou.orders.size shouldBe 2
            wou.orders.map(v => (v.id, v.status)).toList should contain((aliceOrder.id(), OrderStatus.Cancelled.name))
            wou.orders.map(v => (v.id, v.status)).toList should contain((aliceOrder2.id(), OrderStatus.Cancelled.name))
          }

          val aliceOrder3 = mkOrder(alice, wavesUsdPair, OrderType.SELL, 3.waves, 25.usd)
          dex1.api.place(aliceOrder3)
          dex1.api.cancelOrderById(aliceOrder3, Some(alice.publicKey))

          checkMessages(stream, "redisDisconnectionTest2") { wou: WsOrdersUpdate =>
            wou.orders.size shouldBe 1
            wou.orders.map(v => (v.id, v.status)).toList should contain((aliceOrder3.id(), OrderStatus.Cancelled.name))
          }
        }
      }
    }
  }

  private def checkMessages(redisStream: RedisStream[String, String], groupName: String)(check: WsOrdersUpdate => Unit): Unit = {
    redisStream.createReadGroup(groupName)
    eventually {
      val messages = redisStream.read(groupName).asScala.toList.sortBy(v => v._1.getId0).lastOption.map(_._2.asScala).getOrElse(Map.empty)
      messages should not be empty
      val message = messages.toList.sortBy(_._1).map(v => Json.parse(v._2).as[WsServerMessage]).collect {
        case wou: WsOrdersUpdate => wou
      }.lastOption
      check(message.value)
    }.tap(_ => redisStream.removeReadGroup(groupName))
  }

  private def collectMessages(redisStream: RedisStream[String, String], groupName: String): Seq[WsOrdersUpdate] = {
    redisStream.createReadGroup(groupName)
    val updates = new AtomicReference[Seq[WsOrdersUpdate]](Seq.empty)
    eventually {
      val messages =
        redisStream.read(groupName).asScala.toList.sortBy(v => v._1.getId0).flatMap(_._2.asScala)
      messages should not be empty
      val parsedMessages = messages.map(v => Json.parse(v._2).as[WsServerMessage]).collect {
        case wou: WsOrdersUpdate => wou
      }
      updates.updateAndGet(_ ++ parsedMessages) should not be empty
    }.tap(_ => redisStream.removeReadGroup(groupName))
    updates.get()
  }

  override protected val dexInitialSuiteConfig: Config = dexRedisConfig(redisStreamName).withFallback(ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  web-sockets.internal-broadcast.messages-interval = 200ms
       |}""".stripMargin
  ))

  override protected def beforeAll(): Unit = {
    redis.start()
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  private def mkRedisClient(redisPort: Option[Int] = None): RedisClient =
    new RedisClient(RedisSettings(
      address = s"redis://${externalRedisAddress.getHostName}:${redisPort.getOrElse(externalRedisAddress.getPort)}",
      username = "default",
      password = "",
      nettyThreads = 3,
      threads = 1,
      retryAttempts = Int.MaxValue,
      retryInterval = 250,
      keepAlive = true,
      pingConnectionInterval = 500,
      connectionPoolSize = 20,
      connectionMinimumIdleSize = 10
    ))

}
