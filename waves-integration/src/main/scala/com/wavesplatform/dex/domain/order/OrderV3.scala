package com.wavesplatform.dex.domain.order

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

@ApiModel(value = "Order")
case class OrderV3(
  @ApiModelProperty(
    value = "Base58 encoded Sender Public Key",
    dataType = "string",
    example = "226pFho3kqHiCoiQVAUq5MVFkg3KzGLc2zLNsbH8GmE7",
    required = true
  ) senderPublicKey: PublicKey,
  @ApiModelProperty(
    value = "Base58 encoded Matcher Public Key",
    dataType = "string",
    example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8",
    required = true
  ) matcherPublicKey: PublicKey,
  assetPair: AssetPair,
  @ApiModelProperty(
    value = "Order type (sell or buy)",
    dataType = "string",
    example = "sell",
    required = true
  ) orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long,
  @ApiModelProperty(
    name = "matcherFeeAssetId",
    value = "Base58 encoded Matcher fee asset ID. Waves is used if field isn't specified",
    dataType = "string",
    example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
    required = false
  )
  override val feeAsset: Asset,
  @ApiModelProperty(
    value = """Order proofs as Base58 encoded signatures list.
                         If Sender's account doesn't have script, put Order's signature as the first proof
                         Signature = Base58 encoded Curve25519.sign(senderPrivateKey, concat(
                            version,
                            bytesOf(senderPublicKey),
                            bytesOf(matcherPublicKey),
                            bytesOf(amountAsset),
                            bytesOf(priceAsset),
                            bytesOf(orderType), // = 0 for buy, 1 for sell
                            bigEndianBytes(price),
                            bigEndianBytes(amount),
                            bigEndianBytes(timestamp),
                            bigEndianBytes(expiration),
                            bigEndianBytes(matcherFee),
                            bytesOf(matcherFeeAssetId)
                         ))""",
    dataType = "List[string]",
    required = true
  )
  proofs: Proofs
) extends Order {

  @ApiModelProperty(
    value = "Order version, equals to 3",
    dataType = "integer",
    example = "3",
    required = true
  )
  val version: Byte = 3

  override def signature: Array[Byte] = proofs.proofs.head.arr

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(version),
        senderPublicKey,
        matcherPublicKey,
        assetPair.bytes,
        orderType.bytes,
        Longs.toByteArray(price),
        Longs.toByteArray(amount),
        Longs.toByteArray(timestamp),
        Longs.toByteArray(expiration),
        Longs.toByteArray(matcherFee),
        feeAsset.byteRepr
      )
    )

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  @ApiModelProperty(hidden = true)
  override val json: Coeval[JsObject] =
    Coeval.evalOnce {
      val sig = Base58.encode(signature)
      Json.obj(
        "version" -> version,
        "id" -> idStr(),
        "sender" -> senderPublicKey.stringRepr,
        "senderPublicKey" -> Base58.encode(senderPublicKey),
        "matcherPublicKey" -> Base58.encode(matcherPublicKey),
        "assetPair" -> Json.toJsObject(assetPair),
        "orderType" -> orderType.toString,
        "amount" -> amount,
        "price" -> price,
        "timestamp" -> timestamp,
        "expiration" -> expiration,
        "matcherFee" -> matcherFee,
        "matcherFeeAssetId" -> feeAsset.maybeBase58Repr,
        "signature" -> sig,
        "proofs" -> proofs.proofs
      )
    }

}

object OrderV3 extends EntityParser[OrderV3] {

  def buy(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    matcherFeeAssetId: Asset
  ): Order = {

    val unsigned =
      OrderV3(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    matcherFeeAssetId: Asset
  ): Order = {

    val unsigned =
      OrderV3(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    matcherFeeAssetId: Asset
  ): Order = {

    val unsigned = OrderV3(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  override def statefulParse: Stateful[(OrderV3, ConsumedBytesOffset)] =
    for {
      _ <- read[Byte].map(v => if (v != 3) throw new Exception(s"Incorrect order version: expect 3 but found $v"))
      sender <- read[PublicKey]
      matcher <- read[PublicKey]
      amountAsset <- read[Asset]
      priceAsset <- read[Asset]
      orderType <- read[Byte]
      price <- read[Long]
      amount <- read[Long]
      timestamp <- read[Long]
      expiration <- read[Long]
      matcherFee <- read[Long]
      feeAsset <- read[Asset]
      proofs <- read[Proofs]
      offset <- read[ConsumedBytesOffset]
    } yield OrderV3(
      sender,
      matcher,
      AssetPair(amountAsset, priceAsset),
      OrderType(orderType),
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      feeAsset,
      proofs
    ) -> offset

}
