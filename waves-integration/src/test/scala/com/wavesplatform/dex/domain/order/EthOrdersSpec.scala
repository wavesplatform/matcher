package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.AddressScheme
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class EthOrdersSpec extends WavesIntegrationSuiteBase with Matchers with EitherValues {

  "EthOrdersSpec" - {

    "DEX-1768 bug [1]" in {

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = 'W'
      }

      val rawOrder =
        """
          |{
          |  "amount": 211125290,
          |  "amountAsset": "WAVES",
          |  "assetPair": {
          |    "amountAsset": "WAVES",
          |    "priceAsset": "34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ"
          |  },
          |  "eip712Signature": "0x4305a6f070179f7d5fa10557d764373d740ecb24a1177e8c2e01cc03f7c90eda78af2bdc88c964032ed3ae3807eed05c20c981ffe7b30e060f9f145290905b8a1b",
          |  "expiration": 1671111399020,
          |  "matcherFee": 23627,
          |  "matcherFeeAssetId": "34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ",
          |  "matcherPublicKey": "9cpfKN9suPNvfeUNphzxXMjcnn974eme8ZhWUjaktzU5",
          |  "orderType": "buy",
          |  "price": 2357071,
          |  "priceAsset": "34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ",
          |  "timestamp": 1668605799020,
          |  "version": 4
          |} """.stripMargin

      parseRawOrder(rawOrder).senderPublicKey.toAddress.stringRepr shouldBe "3P5sHA8UmYnGW8sQsaFoeJwn7nzjMgyhUdi"
    }

    "DEX-1768 bug [2]" in {

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = 'T'
      }

      val rawOrder =
        """
          |{
          |  "amount": 100000000,
          |  "price": 14781968,
          |  "amountAsset": "WAVES",
          |  "priceAsset": "25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT",
          |  "expiration": 1671026475679,
          |  "matcherFee": 24884,
          |  "matcherFeeAssetId": "25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT",
          |  "orderType": "buy",
          |  "matcherPublicKey": "8QUAqtTckM5B8gvcuP7mMswat9SjKUuafJMusEoSn1Gy",
          |  "version": 4,
          |  "timestamp": 1668520875679,
          |  "assetPair": {
          |    "amountAsset": "WAVES",
          |    "priceAsset": "25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT"
          |  },
          |  "eip712Signature": "0x12f72d3bba93bda930ee5c280e1d39b7e7dcc439d789c92eff40ea860480213a0e79323093c8aee04c2a269de01c7d587a18b02d02746dec75ec1457accb72a301"
          |}
          |""".stripMargin

      parseRawOrder(rawOrder).senderPublicKey.toAddress.stringRepr shouldBe "3N8HNri7zQXVw8Bn9BZKGRpsznNUFXM24zL"
    }
  }

  private def parseRawOrder(rawOrder: String): Order =
    OrderJson.orderReads.reads(Json.parse(rawOrder)).asEither.value

}
