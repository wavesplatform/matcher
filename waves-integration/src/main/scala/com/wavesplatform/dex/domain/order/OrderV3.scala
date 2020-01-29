package com.wavesplatform.dex.domain.order

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.Stateful
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

case class OrderV3(senderPublicKey: PublicKey,
                   matcherPublicKey: PublicKey,
                   assetPair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timestamp: Long,
                   expiration: Long,
                   matcherFee: Long,
                   override val feeAsset: Asset,
                   proofs: Proofs)
    extends Order {

  def version: Byte = 3

  override def signature: Array[Byte] = proofs.proofs.head.arr

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

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      {
        val sig = Base58.encode(signature)
        Json.obj(
          "version"           -> version,
          "id"                -> idStr(),
          "sender"            -> senderPublicKey.stringRepr,
          "senderPublicKey"   -> Base58.encode(senderPublicKey),
          "matcherPublicKey"  -> Base58.encode(matcherPublicKey),
          "assetPair"         -> assetPair.json,
          "orderType"         -> orderType.toString,
          "amount"            -> amount,
          "price"             -> price,
          "timestamp"         -> timestamp,
          "expiration"        -> expiration,
          "matcherFee"        -> matcherFee,
          "matcherFeeAssetId" -> feeAsset.maybeBase58Repr,
          "signature"         -> sig,
          "proofs"            -> proofs.proofs
        )
      }
    )
}

object OrderV3 extends EntityParser[OrderV3] {

  def buy(sender: KeyPair,
          matcher: PublicKey,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long,
          matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(sender: KeyPair,
           matcher: PublicKey,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long,
           matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(sender: KeyPair,
            matcher: PublicKey,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  override def statefulParse: Stateful[OrderV3] = {
    for {
      _           <- read[Byte].map(v => if (v != 3) throw new Exception(s"Incorrect order version: expect 3 but found $v"))
      sender      <- read[PublicKey]
      matcher     <- read[PublicKey]
      amountAsset <- read[Asset]
      priceAsset  <- read[Asset]
      orderType   <- read[Byte]
      price       <- read[Long]
      amount      <- read[Long]
      timestamp   <- read[Long]
      expiration  <- read[Long]
      matcherFee  <- read[Long]
      feeAsset    <- read[Asset]
      proofs      <- read[Proofs]
    } yield
      OrderV3(sender,
              matcher,
              AssetPair(amountAsset, priceAsset),
              OrderType(orderType),
              amount,
              price,
              timestamp,
              expiration,
              matcherFee,
              feeAsset,
              proofs)
  }
}
