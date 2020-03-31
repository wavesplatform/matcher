package com.wavesplatform.it.sync.api.ws

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.StatusCodes
import cats.syntax.option._
import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.headers.{`X-Error-Code`, `X-Error-Message`}
import com.wavesplatform.dex.error.{ApiKeyIsNotValid, RequestInvalidSignature}
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
      wsc.getMessagesBuffer.foreach { x =>
        x.balances should not be empty
      }
    }

    "correctly handle rejections" in {

      val kp = mkKeyPair("JIo6cTep_u3_6ocToHa")

      val timestamp     = System.currentTimeMillis
      val signedMessage = "as".getBytes(StandardCharsets.UTF_8) ++ kp.publicKey.arr ++ Longs.toByteArray(timestamp)

      val correctSignature   = com.wavesplatform.dex.domain.crypto.sign(kp, signedMessage).base58
      val incorrectSignature = "incorrectSignature"

      val incorrectKey = "incorrectKey".some
      val correctKey   = com.wavesplatform.dex.it.docker.apiKey.some
      val withoutKey   = Option.empty[String]

      val uriWithoutParams                    = s"${getBaseBalancesStreamUri(dex1)}${kp.publicKey}"
      def uriWithSignature(signature: String) = s"$uriWithoutParams?t=$timestamp&s=$signature"

      forAll(
        Table(
          // format: off
          ("uri",                                "api-key",    "expected status",             "expected error"),
          (uriWithSignature(incorrectSignature), incorrectKey, StatusCodes.Forbidden,          ApiKeyIsNotValid.some),
          (uriWithSignature(incorrectSignature), withoutKey,   StatusCodes.BadRequest,         RequestInvalidSignature.some),
          (uriWithSignature(incorrectSignature), correctKey,   StatusCodes.SwitchingProtocols, None),
          (uriWithSignature(correctSignature),   incorrectKey, StatusCodes.Forbidden,          ApiKeyIsNotValid.some),
          (uriWithSignature(correctSignature),   withoutKey,   StatusCodes.SwitchingProtocols, None),
          (uriWithSignature(correctSignature),   correctKey,   StatusCodes.SwitchingProtocols, None),
          (uriWithoutParams,                     incorrectKey, StatusCodes.Forbidden,          ApiKeyIsNotValid.some),
          (uriWithoutParams,                     withoutKey,   StatusCodes.BadRequest,         RequestInvalidSignature.some),
          (uriWithoutParams,                     correctKey,   StatusCodes.SwitchingProtocols, None)
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