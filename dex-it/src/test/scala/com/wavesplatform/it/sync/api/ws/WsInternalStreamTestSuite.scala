package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class WsInternalStreamTestSuite extends WsSuiteBase with TableDrivenPropertyChecks {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  web-sockets.internal-broadcast.messages-interval = 100ms
         |}""".stripMargin)
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  override def afterEach(): Unit = dex1.api.cancelAll(alice)

  private def mkWsInternalConnection(): WsConnection = mkWsInternalConnection(dex1)

  "Internal stream should" - {
    "not send message if there is no matches" in {
      val wsc = mkWsInternalConnection()
      wsc.receiveNoMessages()
      wsc.close()
    }

    "send matches" - {
      "one match" in {
        //val acc1 = mkAccountWithBalance(150.usd -> usd, 10.waves -> Waves)
        //val acc2 = mkAccountWithBalance(150.usd -> usd, 10.waves -> Waves)
        val order1 = mkOrderDP(alice, wavesUsdPair, OrderType.SELL, 2.waves, 3)
        val order2 = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 1.waves, 4)

        val wsc = mkWsInternalConnection()

        List(order1, order2).foreach(dex1.api.place)
        val buffer0 = wsc.receiveAtLeastN[WsOrdersUpdate](1)
        buffer0 should have size 1

        buffer0.orderEvents should matchTo(
          Map(
            order1.id() -> List(WsCompleteOrder(
              id = order1.id(),
              timestamp = 0L,
              amountAsset = Waves,
              priceAsset = usd,
              side = OrderType.SELL,
              isMarket = false,
              price = 3,
              amount = 1,
              fee = 3000,
              feeAsset = Waves,
              status = OrderStatus.Filled.name,
              filledAmount = 3,
              filledFee = 0.3.waves,
              avgWeighedPrice = 5,
              eventTimestamp = 0L
            )),
            order2.id() -> List(WsCompleteOrder(
              id = order2.id(),
              timestamp = 0L,
              amountAsset = Waves,
              priceAsset = usd,
              side = OrderType.BUY,
              isMarket = false,
              price = 3,
              amount = 1,
              fee = 3000,
              feeAsset = Waves,
              status = OrderStatus.Filled.name,
              filledAmount = 3,
              filledFee = 0.3.waves,
              avgWeighedPrice = 5,
              eventTimestamp = 0L
            ))
          )
        )

        wsc.close()
      }
    }
  }
}
