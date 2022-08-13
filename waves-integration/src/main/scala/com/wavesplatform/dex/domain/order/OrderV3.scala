package com.wavesplatform.dex.domain.order

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import monix.eval.Coeval

case class OrderV3(
  orderAuthentication: OrderAuthentication,
  matcherPublicKey: PublicKey,
  assetPair: AssetPair,
  orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long,
  override val feeAsset: Asset
) extends Order {

  val version: Byte = 3

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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned =
      OrderV3(oa, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned =
      OrderV3(oa, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    val sig = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV3(oa, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    val sig = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
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
      OrderAuthentication.OrderProofs(sender, proofs),
      matcher,
      AssetPair(amountAsset, priceAsset),
      OrderType(orderType),
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      feeAsset
    ) -> offset

}
