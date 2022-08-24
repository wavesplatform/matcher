package com.wavesplatform.dex.domain.transaction

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction._
import monix.eval.Coeval

import scala.util.Try

case class ExchangeTransactionV2(
  buyOrder: Order,
  sellOrder: Order,
  amount: Long,
  price: Long,
  buyMatcherFee: Long,
  sellMatcherFee: Long,
  fee: Long,
  timestamp: Long,
  proofs: Proofs
) extends ExchangeTransaction {

  override val version: Byte = 2

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(0: Byte, ExchangeTransaction.typeId, version) ++
      Ints.toByteArray(buyOrder.bytes().length) ++ orderMark(buyOrder.version) ++ buyOrder.bytes() ++
      Ints.toByteArray(sellOrder.bytes().length) ++ orderMark(sellOrder.version) ++ sellOrder.bytes() ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())

  override val assetDecimalsPrice: Long = price
}

object ExchangeTransactionV2 extends ExchangeTransactionParser[ExchangeTransactionV2] {

  def mkSigned(
    amountAssetDecimals: Int,
    priceAssetDecimals: Int,
    matcher: PrivateKey,
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long
  ): ExchangeTransactionResult[ExchangeTransactionV2] =
    mk(
      amountAssetDecimals,
      priceAssetDecimals,
      buyOrder,
      sellOrder,
      amount,
      price,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp,
      proofs = Proofs.empty
    ).map { unverified =>
      unverified.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, unverified.bodyBytes())))))
    }

  def mk(
    amountAssetDecimals: Int,
    priceAssetDecimals: Int,
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    proofs: Proofs
  ): ExchangeTransactionResult[ExchangeTransactionV2] =
    ExchangeTransactionResult.fromEither(
      validateExchangeParams(
        2,
        amountAssetDecimals,
        priceAssetDecimals,
        buyOrder,
        sellOrder,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp
      ),
      ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs)
    )

  override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {

    if (bytes.length < 3) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

    val (parsedMark, parsedTypeId, parsedVersion) = bytes.take(3) match {
      case Array(parsedMark, parsedTypeId, parsedVersion) => (parsedMark, parsedTypeId, parsedVersion)
      case _ => throw new IllegalArgumentException(s"Can't parse header=${bytes.mkString("Array(", ", ", ")")}")
    }

    if (parsedMark != 0) throw new IllegalArgumentException(s"Expected the '0' byte, but got '$parsedMark'")
    if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
    if (parsedVersion != 2) throw new IllegalArgumentException(s"Expected version of transaction 2, but got '$parsedVersion'")

    3
  }

  override def statefulParse: Stateful[(ExchangeTransactionV2, ConsumedBytesOffset)] =
    for {
      _ <- read[Int]
      (buyOrder, _) <- Order.statefulParse
      _ <- read[Int]
      (sellOrder, _) <- Order.statefulParse
      price <- read[Long]
      amount <- read[Long]
      buyMatcherFee <- read[Long]
      sellMatcherFee <- read[Long]
      fee <- read[Long]
      timestamp <- read[Long]
      proofs <- read[Proofs]
      offset <- read[ConsumedBytesOffset]
    } yield ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) -> offset

}
