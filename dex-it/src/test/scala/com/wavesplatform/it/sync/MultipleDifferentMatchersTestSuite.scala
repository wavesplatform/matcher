package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.MultipleVersions
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexMultipleVersions

@DexMultipleVersions
class MultipleDifferentMatchersTestSuite extends MatcherSuiteBase with MultipleVersions {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  private val accounts = List(alice, bob)

  "Backward compatibility by order book of v2.0.x" - {
    "if (!submitted.order.isValid(eventTs))" in {}
    "can not match" - {
      "limit" in {}
      "market" in {
        //dex2.api.placeMarket(mkOrder(alice, SELL, 0.0002.waves, 900.usd))
      }
    }

    "can match" - {
      "if (!submitted.isValid(counter.price))" in test {
        dex2.api.place(mkOrder(alice, SELL, 0.0002.waves, 900.usd))
        placeAndAwaitAtDex(mkOrder(bob, BUY, 0.00001.waves, 1000.usd), OrderStatus.Cancelled, dex2)
      }

      "if (!counter.order.isValid(eventTs))" in {}
      "else" - {
        "if (orderExecutedEvent.counterRemaining.isValid)" in {}
        "else" - {
          "limit" in {}
          "market" in {}
        }
      }
    }
  }

  override protected def beforeAll(): Unit = {
    kafka.start()
    wavesNode1.start()
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)
    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
    dex2.start()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    kafka.stop()
  }

  private def test(f: => Any): Unit = {
    cancelAll()
    val stateBefore = ""
    f
    val stateAfter = ""
    stateBefore should matchTo(stateAfter)
    cancelAll()
  }

  private def cancelAll(): Unit = {
    accounts.foreach(dex1.api.cancelAll(_))
    accounts.foreach(dex1.api.waitForOrderHistory(_, activeOnly = Some(true))(_.isEmpty))
  }

  private def mkOrder(account: KeyPair, orderSide: OrderType, amount: Long, price: Long): Order =
    mkOrder(account, wavesUsdPair, orderSide, amount, price)

}
