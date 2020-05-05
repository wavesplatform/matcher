package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.WsSuiteBase

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
