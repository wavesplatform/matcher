package com.wavesplatform.dex.domain.order

import cats.syntax.option._
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.order.OrderOps._
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class OrderJsonSpec extends WavesIntegrationSuiteBase with Matchers {

  private val usd: IssuedAsset = IssuedAsset("USDN".getBytes)
  private val wavesUsdPair: AssetPair = AssetPair(Waves, usd)

  private val senderKeyPair = KeyPair("sender".getBytes)
  private val matcherKeyPair = KeyPair("matcher".getBytes)

  private val order: Order =
    OrderV3(
      senderKeyPair,
      matcherKeyPair,
      wavesUsdPair,
      OrderType.SELL,
      123456789L,
      119L,
      1578074613225L,
      1578077613225L,
      300000L,
      Waves
    )

  def getOrderJson(feeAssetRepresentation: Option[String]): String =
    s"""{
       |  "version" : 3,
       |  "id" : "HCXVwUaETvHySqRyJSZ2NbnLeqTYP2FBkbwecXmMdbM6",
       |  "sender" : "3N7YhNxYuoa59oCCMPuQRqSx6KM61FkGjYa",
       |  "senderPublicKey" : "226pFho3kqHiCoiQVAUq5MVFkg3KzGLc2zLNsbH8GmE7",
       |  "matcherPublicKey" : "J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf",
       |  "assetPair" : {
       |    "amountAsset" : "WAVES",
       |    "priceAsset" : "3BVv85"
       |  },
       |  "orderType" : "sell",
       |  "amount" : 123456789,
       |  "price" : 119,
       |  "timestamp" : 1578074613225,
       |  "expiration" : 1578077613225,
       |  "matcherFee" : 300000,${feeAssetRepresentation.fold("")(str => s"""\n"matcherFeeAssetId" : $str,""")}
       |  "signature" : "7dsUd1bQZFWCEKyYqkPxd5AE7x88QUK6xJH86KcqtE2LHURxN92QEJRfojHkgezez6fppDGvTCUcr4ZmrPRKmTZ",
       |  "proofs" : [ "7dsUd1bQZFWCEKyYqkPxd5AE7x88QUK6xJH86KcqtE2LHURxN92QEJRfojHkgezez6fppDGvTCUcr4ZmrPRKmTZ" ]
       |}""".stripMargin

  "Domain OrderV3 should" - {

    "be deserialized from JSON when" - {

      "matcherFeeAssetId passed as null" in {
        Json.parse(getOrderJson("null".some)).as[Order] updateProofs order.proofs shouldBe order
      }

      "matcherFeeAssetId passed as WAVES" in {
        Json.parse(getOrderJson("\"WAVES\"".some)).as[Order] updateProofs order.proofs shouldBe order
      }

      "matcherFeeAssetId wasn't passed" in {
        Json.parse(getOrderJson(None)).as[Order] updateProofs order.proofs shouldBe order
      }

      "matcherFeeAssetId passed as Issued asset" in {
        Json
          .parse(getOrderJson(s""""${usd.id}"""".some))
          .as[Order]
          .updateProofs(order.proofs)
          .updateFeeAsset(usd) shouldBe order.updateFeeAsset(usd)
      }
    }
  }
}
