package com.wavesplatform.dex.domain.transaction

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.domain.account.PrivateKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{Signature, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.{Authorized, Proofs}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.order.OrderV1
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction._
import com.wavesplatform.dex.domain.utils._
import monix.eval.Coeval
import play.api.libs.json.{JsArray, JsString, JsValue}

import scala.util.Try

case class ExchangeTransactionV1(buyOrder: OrderV1,
                                 sellOrder: OrderV1,
                                 amount: Long,
                                 price: Long,
                                 buyMatcherFee: Long,
                                 sellMatcherFee: Long,
                                 fee: Long,
                                 timestamp: Long,
                                 signature: ByteStr)
    extends ExchangeTransaction
    with Authorized {

  override def version: Byte = 1

  protected override def proofField: Seq[(String, JsValue)] = {
    val sig = JsString(signature.base58)
    Seq(
      "signature" -> sig,
      "proofs"    -> JsArray { Seq(sig) }
    )
  }

  def proofs: Proofs = Proofs.create(Seq(signature)).explicitGet()

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(ExchangeTransaction.typeId) ++
        Ints.toByteArray(buyOrder.bytes().length) ++ Ints.toByteArray(sellOrder.bytes().length) ++
        buyOrder.bytes() ++ sellOrder.bytes() ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
        Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
        Longs.toByteArray(timestamp)
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature.arr)

}

object ExchangeTransactionV1 extends ExchangeTransactionParser[ExchangeTransactionV1] {

  def create(matcher: PrivateKey,
             buyOrder: OrderV1,
             sellOrder: OrderV1,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, ExchangeTransactionV1] = {
    create(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, ByteStr.empty).map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(matcher, unverified.bodyBytes())))
    }
  }

  def create(buyOrder: OrderV1,
             sellOrder: OrderV1,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ExchangeTransactionV1] = {
    validateExchangeParams(
      buyOrder,
      sellOrder,
      amount,
      price,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp
    ).map { _ =>
      ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
    }
  }

  override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
    if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
    val parsedTypeId = bytes.head
    if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
    1
  }

  override def statefulParse: Stateful[ExchangeTransactionV1] = {
    for {
      _              <- read[Int]
      _              <- read[Int]
      buyOrder       <- OrderV1.statefulParse
      sellOrder      <- OrderV1.statefulParse
      price          <- read[Long]
      amount         <- read[Long]
      buyMatcherFee  <- read[Long]
      sellMatcherFee <- read[Long]
      fee            <- read[Long]
      timestamp      <- read[Long]
      signature      <- read[Signature]
    } yield ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
  }
}
