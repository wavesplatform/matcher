package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.WsFullOrder.WsExecutionInfo
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration.DurationInt

class WsInternalStreamTestSuite extends WsSuiteBase with TableDrivenPropertyChecks {

  private val messagesInterval = 100.millis
  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  web-sockets.internal-broadcast.messages-interval = $messagesInterval
         |}""".stripMargin)
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
    dex1.api.upsertRate(usd, 2)
    dex1.api.upsertRate(btc, 0.1)
  }

  override def afterEach(): Unit = List(alice, bob).foreach(dex1.api.cancelAll(_))

  private def mkWsInternalConnection(): WsConnection = mkWsInternalConnection(dex1)

  "Internal stream should" - {
    "not send message if there is no matches or cancels" in {
      val wsc = mkWsInternalConnection()
      wsc.receiveNoMessages()
      wsc.close()
    }

    "send messages" - {
      "one match" in {
        val order1 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 5.waves, 3, matcherFee = 0.004.btc, feeAsset = btc)
        val order2 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 1.waves, 4)

        val wsc = mkWsInternalConnection()

        List(order1, order2).foreach(dex1.api.place)
        dex1.api.waitForOrderStatus(order2, OrderStatus.Filled)

        val buffer = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        buffer should have size 1

        val orderEvents = buffer.orderEvents
        orderEvents.keySet should matchTo(Set(order1.id(), order2.id()))

        orderEvents(order1.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order1,
              OrderStatus.PartiallyFilled,
              filledAmount = 1,
              filledFee = 0.0008,
              avgWeighedPrice = 3,
              executedAmount = 1,
              executedFee = 0.0008,
              executedPrice = 3,
              isMarket = false
            ))
        }

        orderEvents(order2.id()) should matchTo {
          List(
            mkExecutedFullOrder(order2,
                                OrderStatus.Filled,
                                filledAmount = 1,
                                filledFee = 0.003,
                                avgWeighedPrice = 3,
                                executedAmount = 1,
                                executedFee = 0.003,
                                executedPrice = 3,
                                isMarket = false))
        }

        wsc.close()
      }

      "market order match" in {
        val order1 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 1.waves, 3, matcherFee = 0.004.btc, feeAsset = btc)
        val order2 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 2.waves, 3)
        val order3 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 3.waves, 4)

        val wsc = mkWsInternalConnection()

        List(order1, order2).foreach(dex1.api.place)
        placeAndAwaitAtDex(order3, OrderStatus.Filled, isMarketOrder = true)

        val buffer = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        buffer should have size 1

        val orderEvents = buffer.orderEvents
        orderEvents.keySet should matchTo(Set(order1.id(), order2.id(), order3.id()))

        orderEvents(order1.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order1,
              OrderStatus.Filled,
              filledAmount = 1,
              filledFee = 0.004,
              avgWeighedPrice = 3,
              executedAmount = 1,
              executedFee = 0.004,
              executedPrice = 3,
              isMarket = false
            ))
        }

        orderEvents(order2.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order2,
              OrderStatus.Filled,
              filledAmount = 2,
              filledFee = 0.003,
              avgWeighedPrice = 3,
              executedAmount = 2,
              executedFee = 0.003,
              executedPrice = 3,
              isMarket = false
            ))
        }

        orderEvents(order3.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order3,
              OrderStatus.Filled,
              filledAmount = 3,
              filledFee = 0.003,
              avgWeighedPrice = 3,
              executedAmount = 2,
              executedFee = 0.002,
              executedPrice = 3,
              isMarket = true
            ),
            mkExecutedFullOrder(
              order3,
              OrderStatus.PartiallyFilled,
              filledAmount = 1,
              filledFee = 0.001,
              avgWeighedPrice = 3,
              executedAmount = 1,
              executedFee = 0.001,
              executedPrice = 3,
              isMarket = true
            )
          )
        }

        wsc.close()
      }

      "one cancel" in {
        val order = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 5.waves, 3, matcherFee = 0.004.btc, feeAsset = btc)
        val wsc   = mkWsInternalConnection()

        placeAndAwaitAtDex(order)
        cancelAndAwait(bob, order)

        val buffer = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        buffer should have size 1

        val orderEvents = buffer.orderEvents
        orderEvents.keys should have size 1
        orderEvents.keys.head shouldBe order.id()

        orderEvents(order.id()) should matchTo {
          List(
            mkCancelledFullOrder(
              order,
              filledAmount = 0,
              filledFee = 0,
              avgWeighedPrice = 0,
              isMarket = false
            ))
        }

        wsc.close()
      }

      "multiple matches" in {
        val order1 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 2.waves, 3, matcherFee = 0.004.btc, feeAsset = btc)
        val order2 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 2.waves, 2, matcherFee = 0.003.waves, feeAsset = Waves)
        val order3 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 4.waves, 4, matcherFee = 0.003.waves, feeAsset = Waves)
        val orders = List(order1, order2, order3)

        val wsc = mkWsInternalConnection()

        orders.foreach(dex1.api.place)
        orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Filled))

        val buffer      = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        val orderEvents = buffer.orderEvents
        orderEvents.keySet should matchTo(orders.map(_.id()).toSet)

        orderEvents(order1.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order1,
              OrderStatus.Filled,
              filledAmount = 2,
              filledFee = 0.004,
              avgWeighedPrice = 3,
              executedAmount = 2,
              executedFee = 0.004,
              executedPrice = 3,
              isMarket = false
            ))
        }

        orderEvents(order2.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order2,
              OrderStatus.Filled,
              filledAmount = 2,
              filledFee = 0.003,
              avgWeighedPrice = 2,
              executedAmount = 2,
              executedFee = 0.003,
              executedPrice = 2,
              isMarket = false
            ))
        }

        orderEvents(order3.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order3,
              OrderStatus.Filled,
              filledAmount = 4,
              filledFee = 0.003,
              avgWeighedPrice = 2.5,
              executedAmount = 2,
              executedFee = 0.0015,
              executedPrice = 3,
              isMarket = false
            ),
            mkExecutedFullOrder(
              order3,
              OrderStatus.PartiallyFilled,
              filledAmount = 2,
              filledFee = 0.0015,
              avgWeighedPrice = 2,
              executedAmount = 2,
              executedFee = 0.0015,
              executedPrice = 2,
              isMarket = false
            )
          )
        }

        checkItemsOrder(buffer)
        wsc.close()
      }

      "multiple matches and cancel on two asset pairs" in {
        val order1 = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 2.waves, 3, matcherFee = 0.003.waves, feeAsset = Waves)
        val order2 = mkOrderDP(alice, wavesBtcPair, OrderType.SELL, 1.waves, 0.005, matcherFee = 0.003.waves, feeAsset = Waves)
        val order3 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 3.waves, 4, matcherFee = 6.usd, feeAsset = usd)
        val order4 = mkOrderDP(bob, wavesBtcPair, OrderType.BUY, 4.waves, 0.005, matcherFee = 0.004.btc, feeAsset = btc)
        val orders = List(order1, order2, order3, order4)

        val wsc = mkWsInternalConnection()

        orders.foreach(dex1.api.place)
        List(order1, order2).foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Filled))
        cancelAndAwait(alice, order3)
        dex1.api.waitForOrderStatus(order4, OrderStatus.PartiallyFilled)
        Thread.sleep(messagesInterval.toMillis * 2)

        val buffer      = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        val orderEvents = buffer.orderEvents
        orderEvents.keySet should matchTo(orders.map(_.id()).toSet)

        orderEvents(order1.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order1,
              OrderStatus.Filled,
              filledAmount = 2,
              filledFee = 0.003,
              avgWeighedPrice = 3,
              executedAmount = 2,
              executedFee = 0.003,
              executedPrice = 3,
              isMarket = false
            )
          )
        }

        orderEvents(order2.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order2,
              OrderStatus.Filled,
              filledAmount = 1,
              filledFee = 0.003,
              avgWeighedPrice = 0.005,
              executedAmount = 1,
              executedFee = 0.003,
              executedPrice = 0.005,
              isMarket = false
            ))
        }

        orderEvents(order3.id()) should matchTo {
          List(
            mkCancelledFullOrder(
              order3,
              filledAmount = 2,
              filledFee = 4,
              avgWeighedPrice = 3,
              isMarket = false
            ),
            mkExecutedFullOrder(
              order3,
              OrderStatus.PartiallyFilled,
              filledAmount = 2,
              filledFee = 4,
              avgWeighedPrice = 3,
              executedAmount = 2,
              executedFee = 4,
              executedPrice = 3,
              isMarket = false
            )
          )
        }

        orderEvents(order4.id()) should matchTo {
          List(
            mkExecutedFullOrder(
              order4,
              OrderStatus.PartiallyFilled,
              filledAmount = 1,
              filledFee = 0.001,
              avgWeighedPrice = 0.005,
              executedAmount = 1,
              executedFee = 0.001,
              executedPrice = 0.005,
              isMarket = false
            ))
        }

        checkItemsOrder(buffer)
        wsc.close()
      }
    }
  }

  private def mkExecutedFullOrder(order: Order,
                                  status: OrderStatus,
                                  filledAmount: Double,
                                  filledFee: Double,
                                  avgWeighedPrice: Double,
                                  executedAmount: Double,
                                  executedFee: Double,
                                  executedPrice: Double,
                                  isMarket: Boolean): WsFullOrder =
    mkFullOrder(order, status, filledAmount, filledFee, avgWeighedPrice, WsExecutionInfo(executedAmount, executedFee, executedPrice).some, isMarket)

  private def mkCancelledFullOrder(order: Order, filledAmount: Double, filledFee: Double, avgWeighedPrice: Double, isMarket: Boolean): WsFullOrder =
    mkFullOrder(order, OrderStatus.Cancelled, filledAmount, filledFee, avgWeighedPrice, none, isMarket)

  private def mkFullOrder(order: Order,
                          status: OrderStatus,
                          filledAmount: Double,
                          filledFee: Double,
                          avgWeighedPrice: Double,
                          executionInfo: Option[WsExecutionInfo],
                          isMarket: Boolean): WsFullOrder = {
    val amountAssetDecimals = efc.unsafeAssetDecimals(order.assetPair.amountAsset)
    val priceAssetDecimals  = efc.unsafeAssetDecimals(order.assetPair.priceAsset)

    def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizeFee(value: Long): Double    = Denormalization.denormalizeAmountAndFee(value, order.feeAsset).toDouble
    def denormalizePrice(value: Long): Double  = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsFullOrder(
      id = order.id(),
      owner = order.sender.toAddress,
      timestamp = 0L,
      amountAsset = order.assetPair.amountAsset,
      priceAsset = order.assetPair.priceAsset,
      side = order.orderType,
      isMarket = isMarket,
      price = denormalizePrice(order.price),
      amount = denormalizeAmount(order.amount),
      fee = denormalizeFee(order.matcherFee),
      feeAsset = order.feeAsset,
      status = status.name,
      filledAmount = filledAmount,
      filledFee = filledFee,
      avgWeighedPrice = avgWeighedPrice,
      eventTimestamp = 0L,
      executedAmount = executionInfo.map(_.amount),
      executedFee = executionInfo.map(_.fee),
      executionPrice = executionInfo.map(_.price)
    )
  }

  private def checkItemsOrder(buffer: List[WsOrdersUpdate]): Unit = withClue("An order of items are preserved") {
    val matches = buffer.flattenOrders
    matches.zip(matches.tail).foreach {
      case (next, prev) =>
        next.eventTimestamp should be >= prev.eventTimestamp
    }
  }
}