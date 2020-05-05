package com.wavesplatform.it.sync.api.ws

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.StatusCodes._
import cats.syntax.option._
import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.headers.{`X-Error-Code`, `X-Error-Message`}
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.error._
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class WsConnectionTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId" ]"""
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    dex1.start()
  }

  "WS connection should" - {

    "be established" in {
      val wsc = mkWsAddressConnection(alice, dex1)
      wsc.close()
      wsc.messages should not be empty
    }

    "stop send updates after closing by user and resend after user open it again" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)
      val wsc = mkWsAddressConnection(acc, dex1)

      eventually { wsc.balanceChanges should have size 1 }
      wsc.close()

      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.balanceChanges should have size 1

      val wsc2 = mkWsAddressConnection(acc, dex1)
      eventually { wsc2.balanceChanges should have size 1 }
    }

//    "correctly handle rejections (public stream)" in {
//      val wavesEthPair = AssetPair(Waves, eth)
//
//      forAll(
//        Table(
//          // format: off
//          ("pair",       "expected status",      "expected error"),
//          (wavesEthPair, BadRequest, OrderAssetPairReversed(wavesEthPair)),
//          (ethWavesPair, NotFound,   OrderBookStopped(ethWavesPair)),
//          // format: on
//        )
//      ) { (assetPair, expectedStatus, expectedError) =>
//        val connection = mkWsOrderBookConnection(assetPair, dex1)
//        connection.messages
//        val response = Await.result(connection.connectionResponse, 1.second).response
//
//        response.status shouldBe expectedStatus
//
//        response.getHeader(`X-Error-Message`.name).get.value shouldBe expectedError.message.text
//        response.getHeader(`X-Error-Code`.name).get.value shouldBe expectedError.code.toString
//
//        connection.close()
//      }
//    }
  }
}
