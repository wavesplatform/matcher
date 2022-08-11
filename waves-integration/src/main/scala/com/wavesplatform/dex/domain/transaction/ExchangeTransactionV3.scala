package com.wavesplatform.dex.domain.transaction

import cats.syntax.either._
import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction._
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import monix.eval.Coeval

import scala.util.Try

case class ExchangeTransactionV3(
  buyOrder: Order,
  sellOrder: Order,
  amount: Long,
  price: Long,
  assetDecimalsPrice: Long,
  buyMatcherFee: Long,
  sellMatcherFee: Long,
  fee: Long,
  timestamp: Long,
  proofs: Proofs
) extends ExchangeTransaction {

  override val version: Byte = 3

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves.getWavesTransaction))

  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(version) ++
      orderMark(buyOrder.version) ++ buyOrder.bytes() ++
      orderMark(sellOrder.version) ++ sellOrder.bytes() ++
      Longs.toByteArray(price) ++ Longs.toByteArray(assetDecimalsPrice) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp) ++ proofs.bytes()
    )

}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

  //converts asset_decimal price to fixed_decimal
  def convertPrice(price: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): Either[GenericError, Long] =
    Either.catchNonFatal {
      (BigDecimal(price) / BigDecimal(10).pow(priceAssetDecimals - amountAssetDecimals)).toBigInt.bigInteger.longValueExact()
    }.leftMap(_ => GenericError(s"price is not convertible to fixed_decimals $price, $amountAssetDecimals, $priceAssetDecimals"))

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
  ): ExchangeTransactionResult[ExchangeTransactionV3] =
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
      Proofs.empty
    ).map { tx =>
      tx.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, tx.bodyBytes())))))
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
  ): ExchangeTransactionResult[ExchangeTransactionV3] =
    ExchangeTransactionResult.fromEither(
      validateExchangeParams(
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
      ExchangeTransactionV3(
        buyOrder,
        sellOrder,
        amount,
        price,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp,
        proofs
      )
    ).map { tx =>
      convertPrice(price, amountAssetDecimals, priceAssetDecimals).map { fixedPrice =>
        tx.copy(price = fixedPrice)
      }.getOrElse(tx)
    }

  private def validateExchangeParams(
    amountAssetDecimals: Int,
    priceAssetDecimals: Int,
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long
  ): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(fee > 0, (), InsufficientFee())
      _ <- Either.cond(amount > 0, (), NonPositiveAmount(amount, "assets"))
      _ <- Either.cond(amount <= Order.MaxAmount, (), GenericError("amount too large"))
      _ <- Either.cond(price > 0, (), GenericError("price should be > 0"))
      _ <- Either.cond(price <= Order.MaxAmount, (), GenericError("price too large"))
      _ <- Either.cond(sellMatcherFee <= Order.MaxAmount, (), GenericError("sellMatcherFee too large"))
      _ <- Either.cond(buyMatcherFee <= Order.MaxAmount, (), GenericError("buyMatcherFee too large"))
      _ <- Either.cond(fee <= Order.MaxAmount, (), GenericError("fee too large"))
      _ <- Either.cond(buyOrder.orderType == OrderType.BUY, (), GenericError("buyOrder should has OrderType.BUY"))
      _ <- Either.cond(sellOrder.orderType == OrderType.SELL, (), GenericError("sellOrder should has OrderType.SELL"))
      _ <- Either.cond(
        buyOrder.matcherPublicKey == sellOrder.matcherPublicKey,
        (),
        GenericError("buyOrder.matcher should be the same as sellOrder.matcher")
      )
      _ <- Either.cond(buyOrder.assetPair == sellOrder.assetPair, (), GenericError("Both orders should have same AssetPair"))
      _ <- Either.cond(buyOrder.isValid(timestamp), (), OrderValidationError(buyOrder, buyOrder.isValid(timestamp).messages()))
      _ <- Either.cond(sellOrder.isValid(timestamp), (), OrderValidationError(sellOrder, sellOrder.isValid(timestamp).labels.mkString("\n")))
      _ <- Either.cond(price <= buyOrder.price && price >= sellOrder.price, (), GenericError("priceIsValid"))
      _ <- convertPrice(price, amountAssetDecimals, priceAssetDecimals)
    } yield ()

  override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
    if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
    val version = bytes.head
    if (version != 3) throw new IllegalArgumentException(s"Expected version of transaction 3, but got '$version'")
    1
  }

  override private[domain] def statefulParse: Stateful[(ExchangeTransactionV3, ConsumedBytesOffset)] =
    for {
      (buyOrder, _) <- Order.statefulParse
      (sellOrder, _) <- Order.statefulParse
      price <- read[Long]
      assetDecimalsPrice <- read[Long]
      amount <- read[Long]
      buyMatcherFee <- read[Long]
      sellMatcherFee <- read[Long]
      fee <- read[Long]
      timestamp <- read[Long]
      proofs <- read[Proofs]
      offset <- read[ConsumedBytesOffset]
    } yield ExchangeTransactionV3(
      buyOrder,
      sellOrder,
      amount,
      price,
      assetDecimalsPrice,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp,
      proofs
    ) -> offset

}
