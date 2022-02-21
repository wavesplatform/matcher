package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.entities.{WsAddressFlag, WsBalances}
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.WsSuiteBase

import java.util.concurrent.atomic.AtomicLong
import scala.util.Using

class WsAddressStreamRealTimeTestSuite extends WsSuiteBase {

  val account = mkKeyPair("Test")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex {
                    |  price-assets = [ "$UsdId", "WAVES" ]
                    |  address-actor {
                    |    realtime-ws-accounts = [${account.publicKey}]
                    |    ws-messages-interval = 1 hour
                    |  }
                    |}""".stripMargin)
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, account, 100.waves, Waves), mkTransfer(alice, account, 100.usd, usd))
    dex1.start()
  }

  "Address stream should" - {

    val ts = new AtomicLong(System.currentTimeMillis())

    "should receive only one imaginary tx update" in {
      Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc =>
        val order = mkOrder(account, wavesUsdPair, SELL, 2.waves, 2.usd)
        placeAndAwaitAtDex(order)
        placeAndAwaitAtDex(mkOrder(bob, wavesUsdPair, SELL, 2.waves, 2.usd))
        val tx = placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, BUY, 4.waves, 2.usd)).head
        eventually {
          val changes = wsc.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
          withClue(s"changes: $changes") {
            changes.size shouldBe 1
            changes.head.txsData should contain(tx.id())
            changes.head.removedTxs should contain(tx.id())
          }
        }
      }
    }

    "send correct updates when account added to address-actor.realtime-ws-accounts without waiting ws-messages-interval" in {
      Using.resource(mkWsAddressConnection(account, dex1)) { wsc =>

        broadcastAndAwait(mkTransfer(account, alice, 2.waves, Waves))
        placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd))
        placeAndAwaitAtNode(mkOrder(account, wavesUsdPair, SELL, 10.waves, 1.usd))

        eventually {
          wsc.balanceChanges.squashed should matchTo(
            Map(
              usd -> WsBalances(110.0, 0),
              Waves -> WsBalances(87.996, 0.0)
            )
          )
        }
      }
    }

    "send only one update for multiple matches for one order" in
    Using.resource(mkWsAddressConnection(account, dex1)) { wsc =>

      val smallOrders = (1 to 5).map { i =>
        mkOrder(alice, wavesUsdPair, SELL, 2.waves, (1 + (i / 10)).usd, ts = ts.incrementAndGet())
      }
      smallOrders.foreach(dex1.api.place)

      wsc.clearMessages()

      val bigOrder = mkOrder(account, wavesUsdPair, BUY, 15.waves, 1.2.usd)

      dex1.api.place(bigOrder)
      smallOrders.foreach(order => dex1.api.waitForOrderStatus(order, HttpOrderStatus.Status.Filled))

      val updates = wsc.orderChanges.filter(_.id == bigOrder.id())

      updates.size should (be > 0 and be <= 2) // one for creating and one for five matches, or just one update
      updates.last.matchInfo.size shouldBe 5
    }

  }
}
