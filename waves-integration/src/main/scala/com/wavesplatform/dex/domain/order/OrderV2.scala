package com.wavesplatform.dex.domain.order

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import monix.eval.Coeval

/**
 * Order to matcher service for asset exchange
 */
case class OrderV2(
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

  override val version: Byte = 2

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    (version +: senderPublicKey.arr) ++ matcherPublicKey.arr ++
    assetPair.bytes ++ orderType.bytes ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
    Longs.toByteArray(matcherFee)
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())
}

object OrderV2 extends EntityParser[OrderV2] {

  def buy(sender: KeyPair, matcher: PublicKey, pair: AssetPair, amount: Long, price: Long, timestamp: Long, expiration: Long, matcherFee: Long)
    : Order = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV2(oa, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
  }

  def sell(sender: KeyPair, matcher: PublicKey, pair: AssetPair, amount: Long, price: Long, timestamp: Long, expiration: Long, matcherFee: Long)
    : Order = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV2(oa, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee)
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
  ): Order = {
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = OrderV2(oa, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(orderAuthentication = oa.copy(proofs = Proofs(List(ByteStr(sig)))))
  }

  override def statefulParse: Stateful[(OrderV2, ConsumedBytesOffset)] =
    for {
      _ <- read[Byte].map(v => if (v != 2) throw new Exception(s"Incorrect order version: expect 2 but found $v"))
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
      proofs <- read[Proofs]
      offset <- read[ConsumedBytesOffset]
    } yield OrderV2(
      OrderAuthentication.OrderProofs(sender, proofs),
      matcher,
      AssetPair(amountAsset, priceAsset),
      OrderType(orderType),
      amount,
      price,
      timestamp,
      expiration,
      matcherFee
    ) -> offset

}
