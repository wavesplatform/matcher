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
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WsAuthenticatedConnection}
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class WsConnectionTestSuite extends MatcherSuiteBase with HasWebSockets with TableDrivenPropertyChecks {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    dex1.start()
  }

  "WS connection should " - {

    "be established" in {
      val wsc = mkWsAuthenticatedConnection(alice, dex1)
      wsc.close()
      wsc.getMessagesBuffer.foreach { _.balances should not be empty }
    }

    "stop send updates after closing by user and resend after user open it again" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)
      val wsc = mkWsAuthenticatedConnection(acc, dex1)

      eventually { wsc.getBalancesChanges should have size 1 }
      wsc.close()

      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.getBalancesChanges should have size 1

      val wsc2 = mkWsAuthenticatedConnection(acc, dex1)
      eventually { wsc2.getBalancesChanges should have size 1 }
    }

    val wavesEthPair = AssetPair(Waves, eth)

    "correctly handle rejections (public stream)" in forAll(
      Table(
        // format: off
        ("pair",       "expected status",      "expected error"),
        (wavesEthPair, BadRequest, OrderAssetPairReversed(wavesEthPair)),
        (ethWavesPair, NotFound,   OrderBookStopped(ethWavesPair)),
        // format: on
      )
    ) { (assetPair, expectedStatus, expectedError) =>
      val connection = mkWsOrderBookConnection(assetPair, dex1)
      val response   = Await.result(connection.getConnectionResponse, 1.second).response

      response.status shouldBe expectedStatus

      response.getHeader(`X-Error-Message`.name).get.value shouldBe expectedError.message.text
      response.getHeader(`X-Error-Code`.name).get.value shouldBe expectedError.code.toString

      connection.close()
    }

    "correctly handle rejections (private stream)" in {

      val ts = System.currentTimeMillis
      val kp = mkKeyPair("JIo6cTep_u3_6ocToHa")

      val pk        = kp.publicKey
      val a         = pk.toAddress
      val signedMsg = authenticatedStreamSignaturePrefix.getBytes(StandardCharsets.UTF_8) ++ pk.arr ++ Longs.toByteArray(ts)
      val s         = com.wavesplatform.dex.domain.crypto.sign(kp, signedMsg).base58

      val anotherPk = mkKeyPair("6ocToH_u3_JIo6cTepa").publicKey
      val invalidPk = "0OlI1"
//      val invalidA  = Address.fromPublicKey(somePk, 'Z'.toByte) // TODO fix in DEX-701
      val invalidS = "invalidS"

      val incorrectKey = "incorrectKey".some
      val withoutKey   = Option.empty[String]
      val key          = com.wavesplatform.dex.it.docker.apiKey.some

      val uriWithoutParams                                                  = s"${getBaseBalancesStreamUri(dex1)}$a"
      def uriWithParams(addr: Address, pubKey: String, sig: String): String = s"${getBaseBalancesStreamUri(dex1)}$addr?p=$pubKey&t=$ts&s=$sig"

      implicit def pk2string(publicKey: PublicKey): String = publicKey.toString

      forAll(
        Table(
          // format: off
          ("uri",                          "api-key",    "expected status",  "expected error"),
          (uriWithoutParams,               incorrectKey, Forbidden,          ApiKeyIsNotValid.some),
          (uriWithoutParams,               withoutKey,   BadRequest,         AuthIsRequired.some),
          (uriWithoutParams,               key,          SwitchingProtocols, None),

          (uriWithParams(a, pk, s),        incorrectKey, Forbidden,          ApiKeyIsNotValid.some),
          (uriWithParams(a, pk, s),        withoutKey,   SwitchingProtocols, None),
          (uriWithParams(a, pk, s),        key,          SwitchingProtocols, None),

          // TODO fix in DEX-701
//          (uriWithParams(invalidA, pk, s), incorrectKey, NotFound,          ???),
//          (uriWithParams(invalidA, pk, s), withoutKey,   NotFound,          ???),
//          (uriWithParams(invalidA, pk, s), key,          NotFound,          ???),

          (uriWithParams(a, anotherPk, s), incorrectKey, Forbidden,          ApiKeyIsNotValid.some),
          (uriWithParams(a, anotherPk, s), withoutKey,   BadRequest,         AddressAndPublicKeyAreIncompatible(a, anotherPk).some),
          (uriWithParams(a, anotherPk, s), key,          SwitchingProtocols, None),

          (uriWithParams(a, invalidPk, s), incorrectKey, Forbidden,          ApiKeyIsNotValid.some),
          (uriWithParams(a, invalidPk, s), withoutKey,   BadRequest,         UserPublicKeyIsNotValid.some),
          (uriWithParams(a, invalidPk, s), key,          SwitchingProtocols, None),
          
          (uriWithParams(a, pk, invalidS), incorrectKey, Forbidden,          ApiKeyIsNotValid.some),
          (uriWithParams(a, pk, invalidS), withoutKey,   BadRequest,         RequestInvalidSignature.some),
          (uriWithParams(a, pk, invalidS), key,          SwitchingProtocols, None)
          // format: on
        )
      ) { (uri, apiKey, expectedStatus, expectedError) =>
        val connection = new WsAuthenticatedConnection(uri, apiKey)
        val response   = Await.result(connection.getConnectionResponse, 1.second).response

        response.status shouldBe expectedStatus

        expectedError.foreach { error =>
          response.getHeader(`X-Error-Message`.name).get.value shouldBe error.message.text
          response.getHeader(`X-Error-Code`.name).get.value shouldBe error.code.toString
        }

        connection.close()
      }
    }
  }
}
