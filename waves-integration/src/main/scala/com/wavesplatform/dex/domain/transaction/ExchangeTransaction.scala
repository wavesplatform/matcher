package com.wavesplatform.dex.domain.transaction

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proven
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.serialization.ByteAndJsonSerializable
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.{Failure, Try}

trait ExchangeTransaction extends ByteAndJsonSerializable with Proven {

  def buyOrder: Order
  def sellOrder: Order
  def amount: Long
  def price: Long
  def assetDecimalsPrice: Long
  def buyMatcherFee: Long
  def sellMatcherFee: Long
  def fee: Long
  def timestamp: Long
  def version: Byte

  // Set, because is could be one trader
  def traders: Set[Address] = Set(buyOrder.senderPublicKey.toAddress, sellOrder.senderPublicKey.toAddress)

  @ApiModelProperty(
    value = "Transaction ID",
    dataType = "string",
    example = "jmKBAZx3j7dbMmcB5syPvysw324gFbXgqPPCZkJYiDCx"
  )
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto fastHash this.bodyBytes()))

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

  protected def proofField: Seq[(String, JsValue)] = Seq("proofs" -> JsArray(this.proofs.proofs.map(p => JsString(p.base58))))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type" -> typeId,
      "chainId" -> AddressScheme.current.chainId,
      "id" -> id().base58,
      "sender" -> senderAddress.stringRepr,
      "senderPublicKey" -> Base58.encode(sender),
      "fee" -> assetFee._2,
      "feeAssetId" -> assetFee._1.maybeBase58Repr,
      "timestamp" -> timestamp
    ) ++ JsObject(proofField)

  @ApiModelProperty(hidden = true)
  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "order1" -> buyOrder.json(),
      "order2" -> sellOrder.json(),
      "amount" -> amount,
      "price" -> price,
      "assetDecimalsPrice" -> assetDecimalsPrice,
      "buyMatcherFee" -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    )
  }

  override def toString: String = json().toString

  def toPrettyString: String = json.map(Json.prettyPrint).value()

  def canEqual(other: Any): Boolean = other.isInstanceOf[ExchangeTransaction]

  override def equals(other: Any): Boolean = other match {
    case tx: ExchangeTransaction => tx.canEqual(this) && id() == tx.id()
    case _ => false
  }

  override def hashCode(): Int = id().hashCode
}

object ExchangeTransaction {

  type Id = ByteStr

  val typeId: Byte = 7

  private[transaction] def orderMark(version: Byte): Array[Byte] =
    if (version == 1) Array(1: Byte) else Array.emptyByteArray

  def parse(bytes: Array[Byte]): Try[ExchangeTransaction] =
    bytes.headOption
      .fold(Failure(new Exception("Empty array")): Try[ExchangeTransaction]) { firstByte =>
        if (firstByte == 0)
          ExchangeTransactionV2.parseBytes(bytes).map(_._1)
        else if (firstByte == 7)
          ExchangeTransactionV1.parseBytes(bytes).map(_._1)
        else //firstByte = 1
          ExchangeTransactionV3.parseBytes(bytes).map(_._1)
      }

}
