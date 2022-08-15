package com.wavesplatform.dex.domain.transaction

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.protobuf.transaction.{SignedTransaction => PbSignedTransaction}
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
    Coeval.evalOnce {
      val txBytes = PBUtils.encodeDeterministic(this.toPBWaves)
      Array(1: Byte) ++ Longs.toByteArray(assetDecimalsPrice) ++ Ints.toByteArray(txBytes.length) ++ txBytes
    }

}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

  //converts asset_decimal price to fixed_decimal
  def convertPrice(price: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): Either[GenericError, Long] =
    Either.catchNonFatal {
      (BigDecimal(price) / BigDecimal(10).pow(priceAssetDecimals - amountAssetDecimals)).toBigInt.bigInteger.longValueExact()
    }.leftMap(_ => GenericError(s"price is not convertible to FIXED_DECIMALS $price, $amountAssetDecimals, $priceAssetDecimals"))

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
    val parsedMark = bytes.head
    if (parsedMark != 1) throw new IllegalArgumentException(s"Expected mark of transaction 1, but got '$parsedMark'")
    1
  }

  override private[domain] def statefulParse: Stateful[(ExchangeTransactionV3, ConsumedBytesOffset)] = {
    def readTx(signedTx: PbSignedTransaction, assetDecimalsPrice: Long) =
      for {
        tx <- signedTx.transaction.wavesTransaction.toRight(new RuntimeException("The transaction must be specified"))
        _ <- Either.cond(tx.version == 3, Right(()), new RuntimeException("The transaction's version must be 3"))
        fee <- tx.fee.map(_.amount).toRight(new RuntimeException("The transaction's fee must be specified"))
        data <- tx.data.exchange.toRight(new RuntimeException("The transaction's data must be specified"))
        orders <- data.orders.toList.traverse(_.toVanilla).leftMap(err => new RuntimeException(err.message))
        (buy, sell) <- Either.catchNonFatal(Order.splitByType(orders.head, orders(1)))
          .leftMap(_ => new RuntimeException("The transaction's orders is corrupted"))
      } yield ExchangeTransactionV3(
        buy,
        sell,
        data.amount,
        data.price,
        assetDecimalsPrice,
        data.buyMatcherFee,
        data.sellMatcherFee,
        fee,
        tx.timestamp,
        signedTx.proofs.map(_.toVanilla)
      )

    for {
      assetDecimalsPrice <- read[Long]
      pbLen <- read[Int]
      tx <- read[ExchangeTransactionV3](pbLen) {
        PBUtils.decode(_, PbSignedTransaction)
          .flatMap(readTx(_, assetDecimalsPrice))
          .fold(throw _, identity)
      }
      offset <- read[ConsumedBytesOffset]
    } yield tx -> offset
  }

}
