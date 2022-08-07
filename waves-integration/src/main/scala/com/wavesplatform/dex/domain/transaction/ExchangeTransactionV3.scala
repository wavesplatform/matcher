package com.wavesplatform.dex.domain.transaction

import cats.data.State
import cats.syntax.either._
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.ConsumedBytesOffset
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.protobuf.transaction.{SignedTransaction => PbSignedTransaction}
import monix.eval.Coeval
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._

import scala.util.Try

case class ExchangeTransactionV3(
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
}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

  //converts asset_decimal price to fixed_decimal
  def convertPrice(price: Long, amountDecimals: Int, priceDecimals: Int): Either[GenericError, Long] =
    Try {
      (BigDecimal(price) / BigDecimal(10).pow(priceDecimals - amountDecimals)).toBigInt.bigInteger.longValueExact()
    }.toEither.leftMap(x => GenericError(x.getMessage))

  def convertPriceUnsafe(price: Long, amountDecimals: Int, priceDecimals: Int): Long =
    convertPrice(price, amountDecimals, priceDecimals).fold(e => throw new RuntimeException(e.err), identity)

  def createUnsafe(
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
    createUnsafe(
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
      unverified.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, unverified.bodyBytes())))))
    }

  def createUnsafe(
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
  ): ExchangeTransactionResult[ExchangeTransactionV3] = {
    val fixedPrice = convertPriceUnsafe(price, amountAssetDecimals, priceAssetDecimals)
    ExchangeTransactionResult.fromEither(
      ExchangeTransaction.validateExchangeParams(
        buyOrder,
        sellOrder,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp
      ),
      ExchangeTransactionV3(buyOrder, sellOrder, amount, fixedPrice, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs)
    )
  }

  override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
    if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
    val version = bytes.head
    if (version != 3) throw new IllegalArgumentException(s"Expected version of transaction 3, but got '$version'")
    1
  }

  override private[domain] def statefulParse = State[EntityParser.S, (ExchangeTransactionV3, ConsumedBytesOffset)] { s =>
    PbSignedTransaction.parseFrom(s.bytes)
    s.copy(offset = s.bytes.length) -> res
  }

}
