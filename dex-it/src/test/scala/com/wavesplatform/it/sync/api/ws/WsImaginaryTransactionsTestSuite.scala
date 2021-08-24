package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.entities.WsAddressFlag
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsAddressSubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.WsSuiteBase

import scala.util.{Random, Using}

final class WsImaginaryTransactionsTestSuite extends WsSuiteBase {

  "WsImaginaryTransactionsTestSuite" - {

    "should send not observed txs (not) events" in test { account =>
      Using.resource(mkDexWsConnection(dex1)) { wsc1 =>
        wsc1.send(
          WsAddressSubscribe(
            account,
            WsAddressSubscribe.defaultAuthType,
            mkJwt(account),
            Set(WsAddressFlag.ImaginaryTxs)
          )
        )

        Using.resource(mkDexWsConnection(dex1)) { wsc2 =>
          wsc2.send(
            WsAddressSubscribe(
              account,
              WsAddressSubscribe.defaultAuthType,
              mkJwt(account)
            )
          )

          val bid = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 10.usd)
          dex1.api.place(bid)

          1 to 5 foreach { i =>
            withClue(s"iteration $i") {
              val ask = mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, 10.usd, ts = System.currentTimeMillis() + i)
              placeAndAwaitAtDex(ask, HttpOrderStatus.Status.Filled)
              val txId = ByteStr(dex1.api.getTransactionsByOrderId(ask).head.id().bytes())
              eventually {
                val messages1 = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
                messages1.head.txsData.value shouldBe Map(txId -> List(ask.id()))
                messages1.head.removedTxs shouldBe empty
                messages1(1).txsData shouldBe empty
                messages1(1).removedTxs.value shouldBe Set(txId)
              }
              wsc1.clearMessages()
              val messages2 = wsc2.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
              messages2 shouldBe empty
            }
          }
        }
      }
    }
  }

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  web-sockets.external-client-handler.messages-interval = 0ms
         |}""".stripMargin
    )
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
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
