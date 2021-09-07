package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.WsBalances
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.WsSuiteBase

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

  private def mkWsAddressConnection(account: KeyPair): WsConnection = mkWsAddressConnection(account, dex1)

  "Address stream should" - {

    "send correct updates when account added to address-actor.realtime-ws-accounts" in {
      Using.resource(mkWsAddressConnection(account)) { wsc =>

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

  }
}
