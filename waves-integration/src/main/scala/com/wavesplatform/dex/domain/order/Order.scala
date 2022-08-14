package com.wavesplatform.dex.domain.order

import cats.syntax.functor._
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.{Authorized, Proofs, Proven}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.transaction.ExchangeTransactionV3
import com.wavesplatform.dex.domain.validation.Validation
import com.wavesplatform.dex.domain.validation.Validation.booleanOperators
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

/**
 * Order to matcher service for asset exchange
 */
trait Order extends Proven with Authorized {

  def orderAuthentication: OrderAuthentication
  def matcherPublicKey: PublicKey
  def assetPair: AssetPair
  def orderType: OrderType
  def amount: Long
  def price: Long
  def timestamp: Long
  def expiration: Long
  def matcherFee: Long
  def version: Byte
  def feeAsset: Asset = Waves
  def assets: Set[Asset] = Set(assetPair.amountAsset, assetPair.priceAsset, feeAsset)

  import Order._

  lazy val senderPublicKey: PublicKey = orderAuthentication match {
    case OrderAuthentication.OrderProofs(key, _) => key
    case OrderAuthentication.Eip712Signature(sig) => EthOrders.recoverEthSignerKey(this, sig)
  }

  @ApiModelProperty(hidden = true)
  lazy val sender: PublicKey = senderPublicKey

  val proofs: Proofs = orderAuthentication match {
    case OrderAuthentication.OrderProofs(_, proofs) => proofs
    case OrderAuthentication.Eip712Signature(_) => Proofs.empty
  }

  val signature: ByteStr = proofs.toSignature

  val eip712Signature: Option[ByteStr] = orderAuthentication match {
    case OrderAuthentication.OrderProofs(_, _) => None
    case OrderAuthentication.Eip712Signature(sig) => Some(sig)
  }

  def isExecutable(amountAssetDecimals: Int, priceAssetDecimals: Int): Validation =
    ExchangeTransactionV3.convertPrice(
      price,
      amountAssetDecimals,
      priceAssetDecimals
    ).isRight :| "Price is not convertible to fixed decimals format" &&
    eip712SignatureValid

  def isValid(atTime: Long): Validation =
    (amount > 0) :| "amount should be > 0" &&
    (price > 0) :| "price should be > 0" &&
    (amount < MaxAmount) :| "amount too large" &&
    getSpendAmount(amount, price).isRight :| "SpendAmount too large" &&
    (getSpendAmount(amount, price).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
    getReceiveAmount(amount, price).isRight :| "ReceiveAmount too large" &&
    (getReceiveAmount(amount, price).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0" &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime" &&
    eip712SignatureValid

  private val eip712SignatureValid: Validation =
    (eip712Signature.isEmpty || version >= 4) :| "eip712Signature available only since V4" &&
    eip712Signature.forall(es => es.size == 65 || es.size == 129) :| "eip712Signature should be of length 65 or 129"

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto fastHash bodyBytes()))

  @ApiModelProperty(hidden = true)
  val idStr: Coeval[String] = Coeval.evalOnce(id().base58)

  @ApiModelProperty(hidden = true)
  def getReceiveAssetId: Asset = Order.getReceiveAssetId(assetPair, orderType)

  @ApiModelProperty(hidden = true)
  def getSpendAssetId: Asset = Order.getSpendAssetId(assetPair, orderType)

  def getSpendAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Order.getSpendAmountUnsafe(
      orderType,
      matchAmount,
      matchPrice,
      spend =>
        if (getSpendAssetId == Waves && !(spend + matcherFee).isValidLong)
          throw new ArithmeticException("BigInteger out of long range")
    )

  def getSpendAmountUnsafeD(matchAmount: Long, matchPrice: Long): BigDecimal =
    Order.getSpendAmountUnsafeD(orderType, matchAmount, matchPrice)

  def getReceiveAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Order.getReceiveAmount(orderType, matchAmount, matchPrice)

  def getReceiveAmountD(matchAmount: Long, matchPrice: Long): BigDecimal =
    Order.getReceiveAmountD(orderType, matchAmount, matchPrice)

  @ApiModelProperty(hidden = true)
  val json: Coeval[JsObject] = Coeval.evalOnce {
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
    ) ++ (if (version >= 3) Json.obj("matcherFeeAssetId" -> feeAsset.maybeBase58Repr) else JsObject.empty) ++
    (if (version >= 4)
       Json.obj(
         "eip712Signature" -> eip712Signature.map(bs => org.web3j.utils.Numeric.toHexString(bs.arr)),
         "priceMode" -> "assetDecimals"
       )
     else JsObject.empty)
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
        feeAsset == o.feeAsset &&
        java.util.Arrays.equals(eip712Signature.getOrElse(ByteStr.empty), o.eip712Signature.getOrElse(ByteStr.empty))
    case _ => false
  }

  override def hashCode(): Int = idStr.hashCode()

  override def toString: String = {
    val feeAssetStr = if (version >= 3) s" feeAsset=${feeAsset.toString}" else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, type=$orderType, amount=$amount, " +
    s"price=$price, ts=$timestamp, exp=$expiration, fee=$matcherFee,$feeAssetStr, eip712Signature=$eip712Signature, proofs=$proofs)"
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
    orderAuthentication: OrderAuthentication,
    matcherPublicKey: PublicKey,
    assetPair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    version: Byte = 1,
    feeAsset: Asset = Asset.Waves
  ): Order =
    version match {
      case 1 =>
        OrderV1(orderAuthentication, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee)
      case 2 =>
        OrderV2(orderAuthentication, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee)
      case 3 =>
        OrderV3(orderAuthentication, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, feeAsset)
      case 4 =>
        OrderV4(orderAuthentication, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, feeAsset)
      case _ =>
        throw new IllegalArgumentException(s"Invalid order version: $version")
    }

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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = version match {
      case 3 | 4 => Order(oa, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, version, feeAsset)
      case _ => Order(oa, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, version)
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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = version match {
      case 3 | 4 => Order(oa, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, version, feeAsset)
      case _ => Order(oa, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, version)
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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = Order(oa, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, version)
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
    val oa = OrderAuthentication.OrderProofs(sender, Proofs.empty)
    val unsigned = Order(oa, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, version, feeAsset)
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
      case 4 => OrderV4
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
          case 4 => OrderV4
          case other => throw new IllegalArgumentException(s"Unexpected order version: $other")
        }
        ep.statefulParse.widen[(Order, ConsumedBytesOffset)]
      }

  def getReceiveAssetId(assetPair: AssetPair, orderType: OrderType): Asset = orderType match {
    case OrderType.BUY => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  def getSpendAssetId(assetPair: AssetPair, orderType: OrderType): Asset = orderType match {
    case OrderType.BUY => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  def getSpendAmountUnsafeD(
    orderType: OrderType,
    matchAmount: Long,
    matchPrice: Long
  ): BigDecimal =
    if (orderType == OrderType.SELL)
      BigDecimal(matchAmount)
    else
      BigDecimal(matchAmount) * matchPrice / PriceConstant

  def getSpendAmountUnsafe(
    orderType: OrderType,
    matchAmount: Long,
    matchPrice: Long,
    validateSpendAmount: BigInt => Unit = (_: BigInt) => ()
  ): Either[ValidationError, Long] =
    Try {
      // We should not correct amount here, because it could lead to fork. See ExchangeTransactionDiff
      if (orderType == OrderType.SELL) matchAmount
      else {
        val spend = BigInt(matchAmount) * matchPrice / PriceConstant
        validateSpendAmount(spend)
        spend.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  def getReceiveAmountD(
    orderType: OrderType,
    matchAmount: Long,
    matchPrice: Long
  ): BigDecimal =
    if (orderType == OrderType.BUY)
      BigDecimal(matchAmount)
    else
      BigDecimal(matchAmount) * matchPrice / PriceConstant

  def getReceiveAmount(
    orderType: OrderType,
    matchAmount: Long,
    matchPrice: Long
  ): Either[ValidationError, Long] =
    Try {
      if (orderType == OrderType.BUY) matchAmount
      else
        (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
    }.toEither.left.map(x => GenericError(x.getMessage))

}
