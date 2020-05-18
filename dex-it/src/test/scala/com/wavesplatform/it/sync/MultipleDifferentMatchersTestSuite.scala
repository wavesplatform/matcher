package com.wavesplatform.it.sync

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.MultipleVersions
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.dex.DexApi
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.MatcherState
import com.wavesplatform.it.tags.DexMultipleVersions

@DexMultipleVersions
class MultipleDifferentMatchersTestSuite extends MatcherSuiteBase with MultipleVersions {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  private val accounts = List(alice, bob)

  "Backward compatibility by order book of v2.0.x" - {
    "if (!submitted.order.isValid(eventTs))" in {}
    "can not match" - {
      "limit" in test {
        val order = mkOrder(bob, SELL, 1.waves, 10.usd)
        dex2.api.place(order)
        waitOnBoth(order, OrderStatus.Accepted)

        Vector(order)
      }

      "market" in test {
        val order1 = mkOrder(bob, SELL, 1.waves, 11.usd)
        val order2 = mkOrder(alice, BUY, 3.waves, 12.usd)

        dex2.api.place(order1)
        dex2.api.placeMarket(order2)

        waitOnBoth(order1, OrderStatus.Filled)
        waitOnBoth(order2, OrderStatus.Filled)

        Vector(order1)
      }
    }

    "can match" - {
      "if (!submitted.isValid(counter.price))" in test {
        val order1 = mkOrder(bob, SELL, 0.0002.waves, 900.usd)
        val order2 = mkOrder(alice, BUY, 0.00001.waves, 1000.usd)

        dex2.api.place(order1)
        dex2.api.place(order2)

        waitOnBoth(order1, OrderStatus.Accepted)
        waitOnBoth(order2, OrderStatus.Cancelled)

        Vector(order1, order2)
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

  private def test(f: => IndexedSeq[Order]): Unit = {
    cancelAll()
    val orders = f
    val state1 = state(dex1.api, orders)
    val state2 = state(dex1.api, orders)
    state1 should matchTo(state2)
    cancelAll()
  }

  private def waitOnBoth(order: Order, status: OrderStatus): Unit = {
    dex1.api.waitForOrderStatus(order, status)
    dex1.api.waitForOrderStatus(order, status)
  }

  private def cancelAll(): Unit = {
    accounts.foreach(dex1.api.cancelAll(_))
    accounts.foreach(dex1.api.waitForOrderHistory(_, activeOnly = Some(true))(_.isEmpty))
  }

  private def mkOrder(account: KeyPair, orderSide: OrderType, amount: Long, price: Long): Order =
    mkOrder(account, wavesUsdPair, orderSide, amount, price)

  private def state(dexApi: DexApi[Id], orders: IndexedSeq[Order]): MatcherState = clean(matcherState(List(wavesUsdPair), orders, accounts, dexApi))

  private def clean(state: MatcherState): MatcherState = state.copy(
    offset = 0L, // doesn't matter in this test
    // we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )

}
