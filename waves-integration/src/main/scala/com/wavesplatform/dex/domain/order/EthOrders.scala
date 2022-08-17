package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.domain.account.{AddressScheme, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto.{ECDSASignature, ECKeyPair, Sign, StructuredDataEncoder}
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.dex.domain.order.OrderOps._

import java.math.BigInteger
import java.nio.ByteBuffer

object EthOrders {

  def signOrder(order: Order, key: ECKeyPair): Order = {
    val message = hashOrderStruct(order)
    val signature = Sign.signMessage(message, key, false)
    val buffer = ByteBuffer.allocate(signature.getR.length + signature.getS.length + signature.getV.length)
    buffer.put(signature.getR)
    buffer.put(signature.getS)
    buffer.put(signature.getV)
    val sig = buffer.array()
    order.updateEip712Signature(sig)
  }

  def recoverEthSignerKey(order: Order, signature: Array[Byte]): PublicKey = {
    val bytes = hashOrderStruct(order)
    recoverEthSignerKey(bytes, signature)
  }

  private def recoverEthSignerKey(message: Array[Byte], signature: Array[Byte]): PublicKey = {
    val signatureData = EthOrders.decodeSignature(signature)
    val signerKey = Sign
      .recoverFromSignature(
        signatureData.getV.head - 27,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        message
      )
      .toByteArray
      .takeRight(64)
    PublicKey(ByteStr(signerKey))
  }

  private def hashOrderStruct(order: Order): Array[Byte] = {
    val json = toEip712Json(order)
    val encoder = new StructuredDataEncoder(json.toString)
    encoder.hashStructuredData()
  }

  private def decodeSignature(signature: Array[Byte]): SignatureData = {
    val buffer = ByteBuffer.wrap(signature)
    val paramSize = buffer.remaining() match {
      case 129 => 64
      case 65 => 32
      case other => throw new IllegalArgumentException(s"Unexpected signature length: $other")
    }
    val R = new Array[Byte](paramSize)
    val S = new Array[Byte](paramSize)
    buffer.get(R)
    buffer.get(S)
    val V = buffer.get()
    new SignatureData(V, R, S)
  }

  private def toEip712Json(order: Order): JsObject = {
    def encodeAsset(asset: Asset): String = asset match {
      case IssuedAsset(id) => id.toString
      case Waves => "WAVES"
    }

    def encodeOrderType(orderType: OrderType): String = orderType match {
      case OrderType.BUY => "BUY"
      case OrderType.SELL => "SELL"
    }

    val message = Json.obj(
      "version" -> order.version.toInt,
      "matcherPublicKey" -> order.matcherPublicKey.toString,
      "amountAsset" -> encodeAsset(order.assetPair.amountAsset),
      "priceAsset" -> encodeAsset(order.assetPair.priceAsset),
      "orderType" -> encodeOrderType(order.orderType),
      "amount" -> order.amount,
      "price" -> order.price,
      "timestamp" -> order.timestamp,
      "expiration" -> order.expiration,
      "matcherFee" -> order.matcherFee,
      "matcherFeeAssetId" -> encodeAsset(order.feeAsset),
      "priceMode" -> "ASSET_DECIMALS"
    )

    Json.parse(orderDomainJson).as[JsObject] ++ Json.obj("message" -> message)
  }

  private def orderDomainJson: String =
    s"""
       |{
       |  "types": {
       |    "EIP712Domain": [
       |      {
       |        "name": "name",
       |        "type": "string"
       |      },
       |      {
       |        "name": "version",
       |        "type": "string"
       |      },
       |      {
       |        "name": "chainId",
       |        "type": "uint256"
       |      }
       |    ],
       |    "Order": [
       |      {
       |        "name": "version",
       |        "type": "int32"
       |      },
       |      {
       |        "name": "matcherPublicKey",
       |        "type": "string"
       |      },
       |      {
       |        "name": "amountAsset",
       |        "type": "string"
       |      },
       |      {
       |        "name": "priceAsset",
       |        "type": "string"
       |      },
       |      {
       |        "name": "orderType",
       |        "type": "string"
       |      },
       |      {
       |        "name": "amount",
       |        "type": "int64"
       |      },
       |      {
       |        "name": "price",
       |        "type": "int64"
       |      },
       |      {
       |        "name": "timestamp",
       |        "type": "int64"
       |      },
       |      {
       |        "name": "expiration",
       |        "type": "int64"
       |      },
       |      {
       |        "name": "matcherFee",
       |        "type": "int64"
       |      },
       |      {
       |        "name": "matcherFeeAssetId",
       |        "type": "string"
       |      },
       |      {
       |        "name": "priceMode",
       |        "type": "string"
       |      }
       |    ]
       |  },
       |  "primaryType": "Order",
       |  "domain": {
       |    "name": "Waves Order",
       |    "version": "1",
       |    "chainId": ${AddressScheme.current.chainId}
       |  },
       |  "message": {}
       |}
       |""".stripMargin

}
