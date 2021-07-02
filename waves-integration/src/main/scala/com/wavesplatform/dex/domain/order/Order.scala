package com.wavesplatform.dex.domain.order

import cats.syntax.functor._
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.{Proofs, Proven}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.serialization.ByteAndJsonSerializable
import com.wavesplatform.dex.domain.validation.Validation
import com.wavesplatform.dex.domain.validation.Validation.booleanOperators
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * Order to matcher service for asset exchange
 */
trait Order extends ByteAndJsonSerializable with Proven {

  def senderPublicKey: PublicKey
  def matcherPublicKey: PublicKey
  def assetPair: AssetPair
  def orderType: OrderType
  def amount: Long
  def price: Long
  def timestamp: Long
  def expiration: Long
  def matcherFee: Long
  def proofs: Proofs
  def version: Byte
  def signature: Array[Byte] = proofs.toSignature
  def feeAsset: Asset = Waves
  def assets: Set[Asset] = Set(assetPair.amountAsset, assetPair.priceAsset, feeAsset)

  import Order._

  @ApiModelProperty(hidden = true)
  val sender: PublicKey = senderPublicKey

  def isValid(atTime: Long): Validation =
    isValidAmount(amount, price) &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime"

  def isValidAmount(matchAmount: Long, matchPrice: Long): Validation =
    (matchAmount > 0) :| "amount should be > 0" &&
    (matchPrice > 0) :| "price should be > 0" &&
    (matchAmount < MaxAmount) :| "amount too large" &&
    getSpendAmount(matchAmount, matchPrice).isRight :| "SpendAmount too large" &&
    (getSpendAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
    getReceiveAmount(matchAmount, matchPrice).isRight :| "ReceiveAmount too large" &&
    (getReceiveAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0"

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto fastHash bodyBytes()))

  @ApiModelProperty(hidden = true)
  val idStr: Coeval[String] = Coeval.evalOnce(id().base58)

  @ApiModelProperty(hidden = true)
  def getReceiveAssetId: Asset = orderType match {
    case OrderType.BUY => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAssetId: Asset = orderType match {
    case OrderType.BUY => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  def getSpendAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Try {
      // We should not correct amount here, because it could lead to fork. See ExchangeTransactionDiff
      if (orderType == OrderType.SELL) matchAmount
      else {
        val spend = BigInt(matchAmount) * matchPrice / PriceConstant
        if (getSpendAssetId == Waves && !(spend + matcherFee).isValidLong)
          throw new ArithmeticException("BigInteger out of long range")
        else spend.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  def getReceiveAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Try {
      if (orderType == OrderType.BUY) matchAmount
      else
        (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
    }.toEither.left.map(x => GenericError(x.getMessage))

  @ApiModelProperty(hidden = true)
  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "version" -> version,
      "id" -> idStr(),
      "sender" -> senderPublicKey.stringRepr,
      "senderPublicKey" -> Base58.encode(senderPublicKey),
      "matcherPublicKey" -> Base58.encode(matcherPublicKey),
      "assetPair" -> Json.toJsObject(assetPair),
      "orderType" -> orderType.toString,
      "amount" -> amount,
      "price" -> price,
      "timestamp" -> timestamp,
      "expiration" -> expiration,
      "matcherFee" -> matcherFee,
      "signature" -> Base58.encode(signature),
      "proofs" -> proofs.proofs
    )
  }

  def jsonStr: String = Json.stringify(json())

  def canEqual(other: Any): Boolean = other.isInstanceOf[Order]

  override def equals(obj: Any): Boolean = obj match {
    case o: Order =>
      o.canEqual(this) &&
        senderPublicKey == o.senderPublicKey &&
        matcherPublicKey == o.matcherPublicKey &&
        assetPair == o.assetPair &&
        orderType == o.orderType &&
        price == o.price &&
        amount == o.amount &&
        timestamp == o.timestamp &&
        expiration == o.expiration &&
        matcherFee == o.matcherFee &&
        proofs == o.proofs &&
        version == o.version &&
        java.util.Arrays.equals(signature, o.signature) &&
        feeAsset == o.feeAsset
    case _ => false
  }

  override def hashCode(): Int = idStr.hashCode()

  override def toString: String = {
    val feeAssetStr = if (version == 3) s" feeAsset=${feeAsset.toString}," else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, tpe=$orderType, amount=$amount, price=$price, ts=$timestamp, exp=$expiration, fee=$matcherFee,$feeAssetStr proofs=$proofs)"
  }

}

object Order extends EntityParser[Order] {

  type Id = ByteStr

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstantExponent: Int = 8
  val PriceConstantDecimal: java.math.BigDecimal = java.math.BigDecimal.ONE.scaleByPowerOfTen(PriceConstantExponent)
  val PriceConstant: Long = PriceConstantDecimal.longValue()
  val MaxAmount: Long = 100 * PriceConstant * PriceConstant

  def apply(
    senderPublicKey: PublicKey,
    matcherPublicKey: PublicKey,
    assetPair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    proofs: Proofs,
    version: Byte = 1,
    feeAsset: Asset = Asset.Waves
  ): Order = version match {
    case 1 => OrderV1(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    case 2 => OrderV2(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    case 3 => OrderV3(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, feeAsset, proofs)
    case _ => throw new IllegalArgumentException(s"Invalid order version: $version")
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def correctAmount(o: Order): Long = correctAmount(o.amount, o.price)

  def buy(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order = {
    val unsigned = version match {
      case 3 => Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, feeAsset)
      case _ => Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    }
    sign(unsigned, sender)
  }

  def sell(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order = {
    val unsigned = version match {
      case 3 => Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, feeAsset)
      case _ => Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    }
    sign(unsigned, sender)
  }

  def apply(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    version: Byte
  ): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def apply(
    sender: KeyPair,
    matcher: PublicKey,
    pair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    version: Byte,
    feeAsset: Asset
  ): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, feeAsset)
    sign(unsigned, sender)
  }

  def sign(unsigned: Order, sender: KeyPair): Order = {
    require(unsigned.senderPublicKey == sender.publicKey)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.updateProofs(Proofs(Seq(ByteStr(sig))))
  }

  def splitByType(o1: Order, o2: Order): (Order, Order) = {
    require(o1.orderType != o2.orderType)
    if (o1.orderType == OrderType.BUY) (o1, o2) else (o2, o1)
  }

  def fromBytes(version: Byte, bytes: Array[Byte]): (Order, ConsumedBytesOffset) = {
    val order = version match {
      case 1 => OrderV1
      case 2 => OrderV2
      case 3 => OrderV3
      case unexpected =>
        throw new RuntimeException(s"unexpected order version $unexpected")
    }
    order.parseBytes(bytes).get
  }

  /** Can be used whenever Order V1 is serialized with prepended version, see [[com.wavesplatform.dex.domain.transaction.ExchangeTransactionV2]] */
  override def statefulParse: Stateful[(Order, ConsumedBytesOffset)] =
    read[Byte]
      .transform { case (s, v) => s.copy(offset = s.offset - (if (v == 1) 0 else 1)) -> v }
      .flatMap { version =>
        val ep = version match {
          case 1 => OrderV1
          case 2 => OrderV2
          case 3 => OrderV3
          case other => throw new IllegalArgumentException(s"Unexpected order version: $other")
        }
        ep.statefulParse.widen[(Order, ConsumedBytesOffset)]
      }

}
