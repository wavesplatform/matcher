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
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction._
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.protobuf.transaction.{SignedTransaction => PbSignedTransaction}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval

import scala.util.Try

@ApiModel(value = "ExchangeTransaction")
case class ExchangeTransactionV3(
  @ApiModelProperty(name = "order1", dataType = "com.wavesplatform.dex.domain.order.OrderV4") buyOrder: Order,
  @ApiModelProperty(name = "order2", dataType = "com.wavesplatform.dex.domain.order.OrderV4") sellOrder: Order,
  @ApiModelProperty() amount: Long,
  @ApiModelProperty() price: Long,
  @ApiModelProperty() assetDecimalsPrice: Long,
  @ApiModelProperty() buyMatcherFee: Long,
  @ApiModelProperty() sellMatcherFee: Long,
  @ApiModelProperty() fee: Long,
  @ApiModelProperty() timestamp: Long,
  @ApiModelProperty(
    value = "Exchange Transaction proofs as Base58 encoded signatures list",
    dataType = "List[string]"
  ) proofs: Proofs
) extends ExchangeTransaction {

  @ApiModelProperty(dataType = "integer", example = "3", allowableValues = "1, 2, 3")
  override val version: Byte = 3

  @ApiModelProperty(hidden = true)
  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPBWaves.getWavesTransaction))

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce {
      val txBytes = PBUtils.encodeDeterministic(this.toPBWaves)
      Array(1: Byte) ++ Longs.toByteArray(assetDecimalsPrice) ++ Ints.toByteArray(txBytes.length) ++ txBytes
    }

}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

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
        3,
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
          .leftMap(_ => new RuntimeException("The transaction's orders are corrupted"))
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
