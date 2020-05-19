package com.wavesplatform.it.sync

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.MultipleVersions
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.dex.DexApi
import com.wavesplatform.it.api.MatcherState
import com.wavesplatform.it.tags.DexMultipleVersions
import com.wavesplatform.it.{MatcherSuiteBase, orderGen}
import org.scalacheck.Gen

@DexMultipleVersions
class MultipleDifferentMatchersTestSuite extends MatcherSuiteBase with MultipleVersions {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  private val carol    = mkKeyPair("carol")
  private val accounts = List(alice, bob)

  "Backward compatibility by order book of v2.0.x" - {
    "OrderBook logic" - {
      "if (!submitted.order.isValid(eventTs))" ignore {} // Hard to reproduce

      "can not match" - {
        "limit" in test {
          val order1 = mkOrder(bob, SELL, 1.waves, 10.usd)
          val order2 = mkOrder(alice, BUY, 2.waves, 9.usd)

          dex2.api.place(order1)
          dex2.api.place(order2)

          waitOnBoth(order1, OrderStatus.Accepted)
          waitOnBoth(order2, OrderStatus.Accepted)

          Vector(order1, order2)
        }

        "market" in test {
          val order1 = mkOrder(bob, SELL, 1.waves, 8.usd)
          val order2 = mkOrder(alice, BUY, 30.waves, 9.usd) // The rest 29 waves can not match

          dex2.api.place(order1)
          dex2.api.placeMarket(order2)

          waitOnBoth(order1, OrderStatus.Filled)
          waitOnBoth(order2, OrderStatus.Filled)

          Vector(order1, order2)
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

        "if (!counter.order.isValid(eventTs))" ignore {} // Hard to reproduce

        "else" - {
          "if (orderExecutedEvent.counterRemaining.isValid)" - {
            "submittedRemaining.isValid" - {
              "limit" ignore {} // Hard to reproduce, DEX-467

              "market" in test {
                val order1 = mkOrder(alice, BUY, 2.waves, 10.usd)
                val order2 = mkOrder(carol, SELL, 2.waves, 10.usd) // The balance of carol is 1.003 waves. This is to simulate afs = 0

                dex2.api.place(order1)
                dex2.api.placeMarket(order2)

                waitOnBoth(order1, OrderStatus.PartiallyFilled)
                waitOnBoth(order2, OrderStatus.Filled)

                Vector(order2, order1)
              }
            }

            "else" - {
              "limit" in test {
                val order1 = mkOrder(bob, SELL, 10.waves, 10.usd)
                val order2 = mkOrder(alice, BUY, 1.waves, 10.usd)

                dex2.api.place(order1)
                dex2.api.place(order2)

                waitOnBoth(order1, OrderStatus.PartiallyFilled)
                waitOnBoth(order2, OrderStatus.Filled)

                Vector(order1, order2)
              }

              "market" in test {
                val order1 = mkOrder(bob, SELL, 10.waves, 11.usd)
                val order2 = mkOrder(alice, BUY, 5.waves, 12.usd)

                dex2.api.place(order1)
                dex2.api.placeMarket(order2)

                waitOnBoth(order1, OrderStatus.PartiallyFilled)
                waitOnBoth(order2, OrderStatus.Filled)

                Vector(order1, order2)
              }
            }
          }

          "else" - {
            "limit" in test {
              val order1 = mkOrder(bob, SELL, 1.waves, 10.usd)
              val order2 = mkOrder(alice, BUY, 2.waves, 10.usd)

              dex2.api.place(order1)
              dex2.api.place(order2)

              waitOnBoth(order1, OrderStatus.Filled)
              waitOnBoth(order2, OrderStatus.PartiallyFilled)

              Vector(order1, order2)
            }

            "market" in test {
              val order1 = mkOrder(bob, SELL, 10.waves, 11.usd)
              val order2 = mkOrder(alice, BUY, 10.waves, 12.usd)

              dex2.api.place(order1)
              dex2.api.placeMarket(order2)

              waitOnBoth(order1, OrderStatus.Filled)
              waitOnBoth(order2, OrderStatus.Filled)

              Vector(order1, order2)
            }
          }
        }
      }
    }

    "random orders" in test {
      val assetPairs = List(ethWavesPair, wavesUsdPair)
      val twoAccountsOrdersGen = Gen.oneOf(
        orderGen(matcher, alice, assetPairs),
        orderGen(matcher, bob, assetPairs)
      )

      val orders = Gen.containerOfN[Vector, Order](200, twoAccountsOrdersGen).sample.get
      orders.foreach(dex2.api.place)

      dex1.api.waitForOrder(orders.last)(_.status != OrderStatus.NotFound)
      dex2.api.waitForOrder(orders.last)(_.status != OrderStatus.NotFound)

      orders
    }
  }

  override protected def beforeAll(): Unit = {
    kafka.start()
    wavesNode1.start()
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)
    broadcastAndAwait(
      IssueUsdTx,
      IssueEthTx,
      mkTransfer(alice, carol, 1.003.waves, Waves)
    )
    wavesNode1.api.waitForHeightArise()
    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)
    broadcastAndAwait(
      mkTransfer(alice, bob, IssueUsdTx.getQuantity / 2, usd),
      mkTransfer(alice, bob, IssueEthTx.getQuantity / 2, eth)
    )
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
    val state2 = state(dex2.api, orders)
    state1 should matchTo(state2)
    cancelAll()
  }

  private def waitOnBoth(order: Order, status: OrderStatus): Unit = {
    dex1.api.waitForOrderStatus(order, status)
    dex2.api.waitForOrderStatus(order, status)
  }

  private def cancelAll(): Unit = {
    accounts.foreach(dex2.api.cancelAll(_))
    accounts.foreach(dex2.api.waitForOrderHistory(_, activeOnly = Some(true))(_.isEmpty))
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
