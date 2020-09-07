package com.wavesplatform.dex.domain.transaction

import cats.syntax.either._
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proven
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.serialization.ByteAndJsonSerializable
import com.wavesplatform.dex.domain.utils._
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.{Failure, Try}

trait ExchangeTransaction extends ByteAndJsonSerializable with Proven {

  def buyOrder: Order
  def sellOrder: Order
  def amount: Long
  def price: Long
  def buyMatcherFee: Long
  def sellMatcherFee: Long
  def fee: Long
  def timestamp: Long
  def version: Byte

  @ApiModelProperty(
    value = "Transaction ID",
    dataType = "string",
    example = "jmKBAZx3j7dbMmcB5syPvysw324gFbXgqPPCZkJYiDCx"
  )
  val id: Coeval[ByteStr] = Coeval.evalOnce { ByteStr(crypto fastHash this.bodyBytes()) }

  @ApiModelProperty(
    name = "senderPublicKey",
    value = "Base58 encoded Sender (Matcher) Public Key",
    dataType = "string",
    example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8"
  )
  override val sender: PublicKey = buyOrder.matcherPublicKey

  @ApiModelProperty(
    name = "sender",
    value = "Base58 encoded Sender (Matcher) Address",
    dataType = "string",
    example = "w8NXgupYEEkif24kbhnV3PEjHv3JGjcWNoG"
  )
  val senderAddress: Address = sender.toAddress

  @ApiModelProperty(
    name = "type",
    value = "Transaction type",
    dataType = "integer",
    example = "7"
  )
  val typeId: Byte = ExchangeTransaction.typeId

  @ApiModelProperty(
    value = "Fee Asset ID, null means WAVES",
    dataType = "string",
    example = "null"
  )
  val feeAssetId: Asset = Waves

  def assetFee: (Asset, Long) = (feeAssetId, fee)
  def chainByte: Option[Byte] = None

  protected def proofField: Seq[(String, JsValue)] = Seq("proofs" -> JsArray { this.proofs.proofs.map(p => JsString(p.base58)) })

  protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> typeId,
      "id"              -> id().base58,
      "sender"          -> senderAddress.stringRepr,
      "senderPublicKey" -> Base58.encode(sender),
      "fee"             -> assetFee._2,
      "feeAssetId"      -> assetFee._1.maybeBase58Repr,
      "timestamp"       -> timestamp
    ) ++ JsObject(proofField)

  @ApiModelProperty(hidden = true)
  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version"        -> version,
      "order1"         -> buyOrder.json(),
      "order2"         -> sellOrder.json(),
      "amount"         -> amount,
      "price"          -> price,
      "buyMatcherFee"  -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    )
  }

  override def toString: String = json().toString

  def toPrettyString: String = json.map(Json.prettyPrint).value()

  override def equals(other: Any): Boolean = other match {
    case tx: ExchangeTransaction => id() == tx.id()
    case _                       => false
  }

  override def hashCode(): Int = id().hashCode
}

object ExchangeTransaction {

  val typeId: Byte = 7

  def parse(bytes: Array[Byte]): Try[ExchangeTransaction] =
    bytes.headOption
      .fold { Failure(new Exception("Empty array")): Try[ExchangeTransaction] } { b =>
        val etp = if (b == 0) ExchangeTransactionV2 else ExchangeTransactionV1; etp parseBytes bytes flatMap { validateExchangeParams(_).foldToTry }
      }

  def validateExchangeParams(tx: ExchangeTransaction): Either[ValidationError, ExchangeTransaction] = {
    validateExchangeParams(tx.buyOrder, tx.sellOrder, tx.amount, tx.price, tx.buyMatcherFee, tx.sellMatcherFee, tx.fee, tx.timestamp).map(_ => tx)
  }

  def validateExchangeParams(buyOrder: Order,
                             sellOrder: Order,
                             amount: Long,
                             price: Long,
                             buyMatcherFee: Long,
                             sellMatcherFee: Long,
                             fee: Long,
                             timestamp: Long): Either[ValidationError, Unit] = {
    Seq(
      (fee <= 0)                                                -> InsufficientFee(),
      (amount <= 0)                                             -> NonPositiveAmount(amount, "assets"),
      (amount > Order.MaxAmount)                                -> GenericError("amount too large"),
      (price <= 0)                                              -> GenericError("price should be > 0"),
      (price > Order.MaxAmount)                                 -> GenericError("price too large"),
      (sellMatcherFee > Order.MaxAmount)                        -> GenericError("sellMatcherFee too large"),
      (buyMatcherFee > Order.MaxAmount)                         -> GenericError("buyMatcherFee too large"),
      (fee > Order.MaxAmount)                                   -> GenericError("fee too large"),
      (buyOrder.orderType != OrderType.BUY)                     -> GenericError("buyOrder should has OrderType.BUY"),
      (sellOrder.orderType != OrderType.SELL)                   -> GenericError("sellOrder should has OrderType.SELL"),
      (buyOrder.matcherPublicKey != sellOrder.matcherPublicKey) -> GenericError("buyOrder.matcher should be the same as sellOrder.matcher"),
      (buyOrder.assetPair != sellOrder.assetPair)               -> GenericError("Both orders should have same AssetPair"),
      (!buyOrder.isValid(timestamp))                            -> OrderValidationError(buyOrder, buyOrder.isValid(timestamp).messages()),
      (!sellOrder.isValid(timestamp))                           -> OrderValidationError(sellOrder, sellOrder.isValid(timestamp).labels.mkString("\n")),
      (!(price <= buyOrder.price && price >= sellOrder.price))  -> GenericError("priceIsValid"),
    ).foldLeft { ().asRight[ValidationError] } { case (result, (hasError, error)) => result.ensure(error)(_ => !hasError) }
  }
}
