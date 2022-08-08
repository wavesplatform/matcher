package com.wavesplatform.dex.domain.transaction

import cats.data.State
import cats.syntax.either._
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.ConsumedBytesOffset
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import monix.eval.Coeval

import scala.util.Try

case class ExchangeTransactionV3(
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
) extends ExchangeTransaction {
  override def version: Byte = 3
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves.getWavesTransaction))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves))

  def withFixedPrice: Either[ValidationError, ExchangeTransactionV3] =
    ExchangeTransactionV3.convertPrice(price, amountAssetDecimals, priceAssetDecimals).map { fixedPrice =>
      copy(price = fixedPrice)
    }

}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

  //converts asset_decimal price to fixed_decimal
  def convertPrice(price: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): Either[GenericError, Long] =
    Either.catchNonFatal {
      (BigDecimal(price) / BigDecimal(10).pow(priceAssetDecimals - amountAssetDecimals)).toBigInt.bigInteger.longValueExact()
    }.leftMap(_ => GenericError(s"price is not convertible to fixed_decimals $price, $amountAssetDecimals, $priceAssetDecimals"))

  def mk(
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
    mkUnsigned(
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
    ).map { unverified =>
      unverified.withFixedPrice
        .map(u => u.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, u.bodyBytes()))))))
        .getOrElse(unverified)
    }

  def mkUnsigned(
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
        proofs
      )
    )

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

  override private[domain] def statefulParse = State[EntityParser.S, (ExchangeTransactionV3, ConsumedBytesOffset)] { s =>
    ???
  }

}
