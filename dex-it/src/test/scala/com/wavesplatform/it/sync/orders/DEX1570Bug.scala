package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType

import java.util.concurrent.atomic.AtomicLong

class DEX1570Bug extends OrderFeeBaseTestSuite {

  "DEX-1570 Bug" - {

    "should do match and produce proper txs" in {
      val buyer = mkKeyPair("buyer")
      val seller = mkKeyPair("seller")
      List(buyer, seller).foreach { acc =>
        broadcastAndAwait(
          mkTransfer(alice, acc, 100.waves, Waves),
          mkTransfer(alice, acc, 100.eth, eth),
          mkTransfer(bob, acc, 100.btc, btc)
        )
      }

      val ts = new AtomicLong(System.currentTimeMillis())
      val bid =
        mkOrder(
          buyer,
          wavesBtcPair,
          OrderType.BUY,
          993423494,
          32661,
          matcherFee = 5230125,
          feeAsset = eth,
          version = 3,
          ts = ts.incrementAndGet()
        )
      val ask1 = mkOrder(
        seller,
        wavesBtcPair,
        OrderType.SELL,
        708279445,
        32499,
        matcherFee = 10460250,
        feeAsset = eth,
        version = 3,
        ts = ts.incrementAndGet()
      )
      val ask2 = mkOrder(
        seller,
        wavesBtcPair,
        OrderType.SELL,
        7041177737L,
        32500,
        matcherFee = 36826240,
        feeAsset = eth,
        version = 3,
        ts = ts.incrementAndGet()
      )
      placeAndAwaitAtDex(ask1)
      placeAndAwaitAtDex(ask2)
      val txs = placeAndAwaitAtNode(bid)

      txs.size shouldBe 2
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueEthTx, IssueBtcTx)
    dex1.start()
    upsertAssetRate(btc -> 0.0003244620597, eth -> 10.460249)
  }

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  allowed-order-versions = [3]
       |  price-assets = [ "$EthId", "$BtcId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      default {
       |        mode = percent
       |        percent {
       |          asset-type = spending
       |          min-fee = 0.1
       |          min-fee-in-waves = 1000000
       |        }
       |      }
       |
       |      discount {
       |        asset = "$EthId"
       |        value = 50
       |      }
       |    }
       |  }
       |}""".stripMargin
  )

}
