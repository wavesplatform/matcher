package com.wavesplatform.dex.domain.order

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Signature, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto._
import monix.eval.Coeval

case class OrderV1(
  orderAuthentication: OrderAuthentication,
  matcherPublicKey: PublicKey,
  assetPair: AssetPair,
  orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long
) extends Order {

  override val version: Byte = 1

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    senderPublicKey ++ matcherPublicKey ++
    assetPair.bytes ++ orderType.bytes ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
    Longs.toByteArray(matcherFee)
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature)
}

object OrderV1 extends EntityParser[OrderV1] {

  def apply(
    senderPublicKey: PublicKey,
    matcherPublicKey: PublicKey,
    assetPair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    signature: Array[Byte]
  ): OrderV1 =
    OrderV1(
      OrderAuthentication.OrderProofs(senderPublicKey, Proofs(List(ByteStr(signature)))),
      matcherPublicKey,
      assetPair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee
    )

  def buy(sender: KeyPair, matcher: PublicKey, pair: AssetPair, amount: Long, price: Long, timestamp: Long, expiration: Long, matcherFee: Long)
    : OrderV1 = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV1(
      oa,
      matcher,
      pair,
      OrderType.BUY,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee
    )
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
  }

  def sell(sender: KeyPair, matcher: PublicKey, pair: AssetPair, amount: Long, price: Long, timestamp: Long, expiration: Long, matcherFee: Long)
    : OrderV1 = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV1(
      oa,
      matcher,
      pair,
      OrderType.SELL,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee
    )
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
    matcherFee: Long
  ): OrderV1 = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV1(
      oa,
      matcher,
      pair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee
    )
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
  }

  override def statefulParse: Stateful[(OrderV1, ConsumedBytesOffset)] =
    for {
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
      signature <- read[Signature]
      offset <- read[ConsumedBytesOffset]
    } yield OrderV1(
      sender,
      matcher,
      AssetPair(amountAsset, priceAsset),
      OrderType(orderType),
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      signature
    ) -> offset

}
