package com.wavesplatform.it.sync.compat

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.it.orderGen
import com.wavesplatform.it.tags.{DexItExternalKafkaRequired, DexMultipleVersions}
import org.scalacheck.Gen

@DexMultipleVersions
@DexItExternalKafkaRequired
class OrderBookBackwardCompatTestSuite extends BackwardCompatSuiteBase with HasKafka {
  "Backward compatibility by order book of v2.0.x" - {
    "OrderBook logic" - {
      "if (!submitted.order.isValid(eventTs))" ignore {} // Hard to reproduce

      "can not match" - {
        "limit" in test {
          val order1 = mkOrder(bob, SELL, 1.waves, 10.usd)
          val order2 = mkOrder(alice, BUY, 2.waves, 9.usd)

          dex2.api.place(order1)
          dex2.api.place(order2)

          waitOnBoth(order1, Status.Accepted)
          waitOnBoth(order2, Status.Accepted)

          Vector(order1, order2)
        }

        "market" in test {
          val order1 = mkOrder(bob, SELL, 1.waves, 8.usd)
          val order2 = mkOrder(alice, BUY, 30.waves, 9.usd) // The rest 29 waves can not match

          dex2.api.place(order1)
          dex2.api.placeMarket(order2)

          waitOnBoth(order1, Status.Filled)
          waitOnBoth(order2, Status.Filled)

          Vector(order1, order2)
        }
      }

      "can match" - {
        "if (!submitted.isValid(counter.price))" in test {
          val order1 = mkOrder(bob, SELL, 0.0002.waves, 900.usd)
          val order2 = mkOrder(alice, BUY, 0.00001.waves, 1000.usd)

          dex2.api.place(order1)
          dex2.api.place(order2)

          waitOnBoth(order1, Status.Accepted)
          waitOnBoth(order2, Status.Cancelled)

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

                waitOnBoth(order1, Status.PartiallyFilled)
                waitOnBoth(order2, Status.Filled)

                Vector(order2, order1)
              }
            }

            "else" - {
              "limit" in test {
                val order1 = mkOrder(bob, SELL, 10.waves, 10.usd)
                val order2 = mkOrder(alice, BUY, 1.waves, 10.usd)

                dex2.api.place(order1)
                dex2.api.place(order2)

                waitOnBoth(order1, Status.PartiallyFilled)
                waitOnBoth(order2, Status.Filled)

                Vector(order1, order2)
              }

              "market" in test {
                val order1 = mkOrder(bob, SELL, 10.waves, 11.usd)
                val order2 = mkOrder(alice, BUY, 5.waves, 12.usd)

                dex2.api.place(order1)
                dex2.api.placeMarket(order2)

                waitOnBoth(order1, Status.PartiallyFilled)
                waitOnBoth(order2, Status.Filled)

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

              waitOnBoth(order1, Status.Filled)
              waitOnBoth(order2, Status.PartiallyFilled)

              Vector(order1, order2)
            }

            "market" in test {
              val order1 = mkOrder(bob, SELL, 10.waves, 11.usd)
              val order2 = mkOrder(alice, BUY, 10.waves, 12.usd)

              dex2.api.place(order1)
              dex2.api.placeMarket(order2)

              waitOnBoth(order1, Status.Filled)
              waitOnBoth(order2, Status.Filled)

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

      dex1.api.waitForOrder(orders.last)(_.status != Status.NotFound)
      dex2.api.waitForOrder(orders.last)(_.status != Status.NotFound)

      orders
    }
  }

  private def test(f: => IndexedSeq[Order]): Unit = {
    cancelAll()
    val orders = f
    val state1 = state(dex1.api, orders)
    val state2 = state(dex2.api, orders)
    state1 should matchTo(state2)
    cancelAll()
  }

  private def mkOrder(account: KeyPair, orderSide: OrderType, amount: Long, price: Long): Order =
    mkOrder(account, wavesUsdPair, orderSide, amount, price)

  override protected def kafkaServer: Option[String] = Some(s"$kafkaIp:9092")

  override protected def beforeAll(): Unit = {
    kafka.start()
    super.beforeAll()
    dex1.start()
    dex2.start()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    kafka.stop()
  }
}
