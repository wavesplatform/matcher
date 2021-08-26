package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.order.{OrderType, OrderV1}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpSuccessfulPlaceSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json = """{
                       |  "message" : {
                       |    "version" : 1,
                       |    "id" : "6KUFD9a6acDVM2Y8wqiFrdt35FPE1WF5QgjpPJqtKrP9",
                       |    "sender" : "3MyE3MtSi6MkXLCu1m7fCsUoJYRt4iibUGS",
                       |    "senderPublicKey" : "CsoEGd7UrxjBWEdoXbwh1x5fT64NNP8nBJ65xuJRx851",
                       |    "matcherPublicKey" : "G67KDhqHdjNNb2tnHRgNbDppQEM9ySXdiBip577n2Xoj",
                       |    "assetPair" : {
                       |      "amountAsset" : "6RQYnag6kTXaoGi3yPmX9JMpPya8WQntSohisKKCMGr",
                       |      "priceAsset" : "8pBLJDEwWgrxxHHnVLYbjhUySKy1qteiZGqK8dJeHUA"
                       |    },
                       |    "orderType" : "buy",
                       |    "amount" : 7235959396154477,
                       |    "price" : 399,
                       |    "timestamp" : 1580225937287,
                       |    "expiration" : 1582677054923,
                       |    "matcherFee" : 5273291907571414,
                       |    "signature" : "5GHs8aQQ3pMEipyotwz6NyzzAQfxizRpzFg7vBsptkQDhxR58cuzkwL2P7XtkTB7KQU7vq6GyZnJTWab2bK3Nixj",
                       |    "proofs" : [ "5GHs8aQQ3pMEipyotwz6NyzzAQfxizRpzFg7vBsptkQDhxR58cuzkwL2P7XtkTB7KQU7vq6GyZnJTWab2bK3Nixj" ]
                       |  },
                       |  "success" : true,
                       |  "status" : "OrderAccepted"
                       |}""".stripMargin

  private val order = OrderV1(
    senderPublicKey = PublicKey(Base58.decode("CsoEGd7UrxjBWEdoXbwh1x5fT64NNP8nBJ65xuJRx851")),
    matcherPublicKey = PublicKey(Base58.decode("G67KDhqHdjNNb2tnHRgNbDppQEM9ySXdiBip577n2Xoj")),
    assetPair = AssetPair(
      amountAsset = IssuedAsset(ByteStr.decodeBase58("6RQYnag6kTXaoGi3yPmX9JMpPya8WQntSohisKKCMGr").get),
      priceAsset = IssuedAsset(ByteStr.decodeBase58("8pBLJDEwWgrxxHHnVLYbjhUySKy1qteiZGqK8dJeHUA").get)
    ),
    orderType = OrderType.BUY,
    amount = 7235959396154477L,
    price = 399L,
    timestamp = 1580225937287L,
    expiration = 1582677054923L,
    matcherFee = 5273291907571414L,
    signature = Base58.decode("5GHs8aQQ3pMEipyotwz6NyzzAQfxizRpzFg7vBsptkQDhxR58cuzkwL2P7XtkTB7KQU7vq6GyZnJTWab2bK3Nixj")
  )

  private val message = HttpSuccessfulPlace(order)

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpSuccessfulPlace] should matchTo(message)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
