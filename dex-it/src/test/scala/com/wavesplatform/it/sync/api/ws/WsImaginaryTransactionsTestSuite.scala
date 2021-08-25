package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.entities.WsAddressFlag
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.duration._
import scala.util.{Random, Using}

final class WsImaginaryTransactionsTestSuite extends WsSuiteBase with HasKafka {

  "WsImaginaryTransactionsTestSuite" - {

    "should send 'not' (not observed txs) events" in test { account =>
      Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc1 =>
        Using.resource(mkWsAddressConnection(account, dex1)) { wsc2 =>
          val bid = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 10.usd)
          dex1.api.place(bid)

          1 to 5 foreach { i =>
            val ask = mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, 10.usd, ts = System.currentTimeMillis() + i)
            placeAndAwaitAtDex(ask, HttpOrderStatus.Status.Filled)
            val txId = ByteStr(dex1.api.getTransactionsByOrderId(ask).head.id().bytes())
            eventually {
              val notObservedTxs = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
              withClue(s"it=$i, txId=$txId, askId=${ask.id()}, messages1=$notObservedTxs") {
                notObservedTxs.size shouldBe 2
                notObservedTxs.head.txsData.value shouldBe Map(txId -> List(ask.id()))
                notObservedTxs.head.removedTxs shouldBe empty
                notObservedTxs(1).txsData shouldBe empty
                notObservedTxs(1).removedTxs.value shouldBe Set(txId)
              }
            }
            wsc1.clearMessages()
            val messages2 = wsc2.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
            messages2 shouldBe empty
          }
        }
      }
    }

    "should send 'nct' (not created txs) events" in test { account =>
      Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc1 =>
        dex1.blockKafkaTraffic()

        val bid = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 10.usd)
        dex2.api.place(bid)
        val ask = mkOrder(account, wavesUsdPair, OrderType.SELL, 5.waves, 10.usd)
        placeAndAwaitAtDex(ask, HttpOrderStatus.Status.Filled, dex2)
        val txId = ByteStr(dex2.api.getTransactionsByOrderId(ask).head.id().bytes())
        eventually {
          val notCreatedTxs = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs)
          withClue(s"txId=$txId, askId=${ask.id()}, messages1=$notCreatedTxs") {
            notCreatedTxs.size shouldBe 1
            notCreatedTxs.head.txsData.value shouldBe Map(txId -> List(ask.id()))
            notCreatedTxs.head.removedTxs shouldBe empty
          }
        }

        Thread.sleep(5.seconds.toMillis)
        val removedNct = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs).flatMap(_.removedTxs).flatten
        removedNct shouldBe empty

        dex1.unblockKafkaTraffic()

        eventually {
          val removedNct = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs).flatMap(_.removedTxs).flatten
          withClue(s"txId=$txId, askId=${ask.id()}, removedNct=$removedNct") {
            removedNct.size shouldBe 1
            removedNct shouldBe List(txId)
          }
        }
      }
    }
  }

  override protected lazy val dexRunConfig: Config = dexKafkaConfig().withFallback(jwtPublicKeyConfig)

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  web-sockets.external-client-handler.messages-interval = 0ms
         |}""".stripMargin
    )
    .withFallback(jwtPublicKeyConfig)

  protected lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    kafka.start()
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
    dex2.start()
  }

  def test[A](f: KeyPair => A): Unit = {
    val account = mkKeyPair(Random.alphanumeric.take(10).mkString(""))
    broadcastAndAwait(
      mkTransfer(alice, account, 100.waves, Waves),
      mkTransfer(alice, account, 100.usd, usd)
    )
    f(account)
  }

}
