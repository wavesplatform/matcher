package com.wavesplatform.dex.domain.transaction

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.PBUtils
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction._
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval

import scala.util.Try

@ApiModel(value = "ExchangeTransaction")
case class ExchangeTransactionV3(
  @ApiModelProperty(name = "order1", dataType = "com.wavesplatform.dex.domain.order.OrderV3") buyOrder: Order,
  @ApiModelProperty(name = "order2", dataType = "com.wavesplatform.dex.domain.order.OrderV3") sellOrder: Order,
  @ApiModelProperty() amount: Long,
  @ApiModelProperty() price: Long,
  @ApiModelProperty() buyMatcherFee: Long,
  @ApiModelProperty() sellMatcherFee: Long,
  @ApiModelProperty() fee: Long,
  @ApiModelProperty() timestamp: Long,
  @ApiModelProperty(
    value = "Exchange Transaction proofs as Base58 encoded signatures list",
    dataType = "List[string]"
  ) proofs: Proofs,
  assetDecimalsPrice: Long
) extends ExchangeTransaction {
  import ExchangeTransactionV3._

  @ApiModelProperty(dataType = "integer", example = "3", allowableValues = "1, 2, 3")
  override val version: Byte = 3

  @ApiModelProperty(hidden = true)
  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      PBUtils.encodeDeterministic(this.toPBSigned.getWavesTransaction) // according to node
    )

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    PBUtils.encodeDeterministic(this.toPBSigned)
  )

  override val bytesForLevelDB: Coeval[Array[Byte]] = Coeval.evalOnce(
    Array(2: Byte, ExchangeTransaction.typeId, version) ++
    Ints.toByteArray(buyOrder.bytes().length) ++ orderMark(buyOrder.version) ++ buyOrder.bytes() ++
    Ints.toByteArray(sellOrder.bytes().length) ++ orderMark(sellOrder.version) ++ sellOrder.bytes() ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
    Longs.toByteArray(timestamp) ++ proofs.bytes() ++ Longs.toByteArray(assetDecimalsPrice)
  )

}

object ExchangeTransactionV3 extends ExchangeTransactionParser[ExchangeTransactionV3] {

  private def orderMark(version: Byte): Array[Byte] = if (version == 1) Array(1: Byte) else Array()

  def create(
    matcher: PrivateKey,
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    assetDecimalsPrice: Long
  ): ExchangeTransactionResult[ExchangeTransactionV3] =
    create(
      buyOrder,
      sellOrder,
      amount,
      price,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp,
      Proofs.empty,
      assetDecimalsPrice
    ).map { unverified =>
      unverified.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, unverified.bodyBytes())))))
    }

  def create(
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    proofs: Proofs,
    assetDecimalsPrice: Long
  ): ExchangeTransactionResult[ExchangeTransactionV3] =
    ExchangeTransactionResult.fromEither(
      validateExchangeParams(
        buyOrder,
        sellOrder,
        amount,
        assetDecimalsPrice,
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
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp,
        proofs,
        assetDecimalsPrice
      )
    )

  override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {

    if (bytes.length < 3) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

    val (parsedMark, parsedTypeId, parsedVersion) = bytes.take(3) match {
      case Array(parsedMark, parsedTypeId, parsedVersion) => (parsedMark, parsedTypeId, parsedVersion)
      case _ => throw new IllegalArgumentException(s"Can't parse header=$bytes")
    }

    if (parsedMark != 2) throw new IllegalArgumentException(s"Expected the '2' byte, but got '$parsedMark'")
    if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
    if (parsedVersion != 3) throw new IllegalArgumentException(s"Expected version of transaction 3, but got '$parsedVersion'")

    3
  }

  override def statefulParse: Stateful[(ExchangeTransactionV3, ConsumedBytesOffset)] =
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
      fixedDecimalsPrice <- read[Long]
      offset <- read[ConsumedBytesOffset]
    } yield ExchangeTransactionV3(
      buyOrder,
      sellOrder,
      amount,
      price,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp,
      proofs,
      fixedDecimalsPrice
    ) -> offset

}
