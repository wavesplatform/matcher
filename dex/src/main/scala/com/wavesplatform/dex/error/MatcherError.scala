package com.wavesplatform.dex.error

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.dex.error.Class.{common => commonClass, _}
import com.wavesplatform.dex.error.Entity.{common => commonEntity, _}
import com.wavesplatform.dex.error.Implicits._
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.settings.{DeviationsSettings, OrderRestrictionsSettings}
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import play.api.libs.json.{JsObject, Json}

sealed abstract class MatcherError(val code: Int, val message: MatcherErrorMessage) extends Product with Serializable {
  def this(obj: Entity, part: Entity, klass: Class, message: MatcherErrorMessage) = this(
    (obj.code << 20) + (part.code << 8) + klass.code, // 32 bits = (12 for object) + (12 for part) + (8 for class)
    message
  )

  override def toString: String = s"${getClass.getCanonicalName}(error=$code)"
}

object MatcherError {
  implicit final class Ops(val self: MatcherError) extends AnyVal {
    def toJson = {
      val obj           = self.message
      val wrappedParams = if (obj.params == JsObject.empty) obj.params else Json.obj("params" -> obj.params)
      Json
        .obj(
          "error"    -> self.code,
          "message"  -> obj.text,
          "template" -> obj.template
        )
        .deepMerge(wrappedParams)
    }
  }
}

case class Amount(asset: Asset, volume: BigDecimal)
object Amount {
  private[error] def apply(asset: Asset, volume: Long)(implicit efc: ErrorFormatterContext): Amount =
    new Amount(asset, Denormalization.denormalizeAmountAndFee(volume, efc.assetDecimals(asset)))
}

case class Price(assetPair: AssetPair, volume: BigDecimal)
object Price {
  private[error] def apply(assetPair: AssetPair, volume: Long)(implicit efc: ErrorFormatterContext): Price =
    new Price(assetPair, Denormalization.denormalizePrice(volume, efc.assetDecimals(assetPair.amountAsset), efc.assetDecimals(assetPair.priceAsset)))
}

case class MatcherErrorMessage(text: String, template: String, params: JsObject)

case object MatcherIsStarting     extends MatcherError(commonEntity, commonEntity, starting, e"System is starting")
case object MatcherIsStopping     extends MatcherError(commonEntity, commonEntity, stopping, e"System is shutting down")
case object RequestTimeout        extends MatcherError(request, commonEntity, stopping, e"Request timed out. Please retry later")
case object FeatureNotImplemented extends MatcherError(commonEntity, feature, unsupported, e"This feature is not implemented")
case object FeatureDisabled       extends MatcherError(commonEntity, feature, disabled, e"This feature is disabled, contact with the administrator")

case class OrderBookBroken(theAssetPair: AssetPair)
    extends MatcherError(orderBook,
                         commonEntity,
                         broken,
                         e"The order book for ${'assetPair -> theAssetPair} is unavailable, please contact with the administrator")

case class OrderBookUnexpectedState(assetPair: AssetPair)
    extends MatcherError(orderBook,
                         commonEntity,
                         unexpected,
                         e"The order book for ${'assetPair -> assetPair} is unexpected state, please contact with the administrator")

case class OrderBookStopped(assetPair: AssetPair)
    extends MatcherError(orderBook,
                         commonEntity,
                         disabled,
                         e"The order book for ${'assetPair -> assetPair} is stopped, please contact with the administrator")

case object CanNotPersistEvent
    extends MatcherError(commonEntity, producer, broken, e"Can not persist event, please retry later or contact with the administrator")

case object CancelRequestIsIncomplete extends MatcherError(request, commonEntity, unexpected, e"Either timestamp or orderId must be specified")

case class UnexpectedMatcherPublicKey(required: PublicKey, given: PublicKey)
    extends MatcherError(
      commonEntity,
      pubKey,
      unexpected,
      e"The required matcher public key for this DEX is ${'required -> required}, but given ${'given -> given}"
    )

case class UnexpectedSender(required: Address, given: Address)
    extends MatcherError(order, pubKey, unexpected, e"The sender ${'given -> given} does not match expected ${'required -> required}")

case class OrderInvalidSignature(id: Order.Id, details: String)
    extends MatcherError(order, signature, commonClass, e"The signature of order ${'id -> id} is invalid: ${'details -> details}")

case class UnexpectedFeeAsset(required: Set[Asset], given: Asset)
    extends MatcherError(
      order,
      fee,
      unexpected,
      if (AssetPair.assetIdStr(given) == "WAVES")
        e"""Required one of the following fee asset: ${'required  -> required}. But given "WAVES" as Base58 string. Remove this field if you want to specify WAVES in JSON"""
      else e"Required one of the following fee asset: ${'required -> required}. But given ${'given -> given}"
    )

case class FeeNotEnough(required: Amount, given: Amount)
    extends MatcherError(
      order,
      fee,
      notEnough,
      e"Required ${'required -> required} as fee for this order, but given ${'given -> given}"
    )
object FeeNotEnough {
  def apply(required: Long, given: Long, asset: Asset)(implicit efc: ErrorFormatterContext): FeeNotEnough = {
    val decimals = efc.assetDecimals(asset)
    new FeeNotEnough(
      required = Amount(asset, Denormalization.denormalizeAmountAndFee(required, decimals)),
      given = Amount(asset, Denormalization.denormalizeAmountAndFee(given, decimals))
    )
  }
}

case class AssetNotFound(theAsset: IssuedAsset) extends MatcherError(asset, commonEntity, notFound, e"The asset ${'assetId -> theAsset} not found")

case class CanNotCreateExchangeTransaction(details: String)
    extends MatcherError(exchangeTx, order, commonClass, e"Can't verify the order by an exchange transaction: ${'details -> details}")

case class WrongExpiration(currentTs: Long, minExpirationOffset: Long, givenExpiration: Long)
    extends MatcherError(
      order,
      expiration,
      notEnough,
      e"""The expiration should be at least
                     |${'currentTimestamp -> currentTs} + ${'minExpirationOffset -> minExpirationOffset} = ${'minExpiration -> (currentTs + minExpirationOffset)},
                     |but it is ${'given -> givenExpiration}"""
    )

case class InvalidJson(fields: List[String])
    extends MatcherError(
      request,
      commonEntity,
      broken,
      if (fields.isEmpty) e"The provided JSON is invalid. Check the documentation"
      else e"The provided JSON contains invalid fields: ${'invalidFields -> fields}. Check the documentation"
    )

case class OrderCommonValidationFailed(details: String)
    extends MatcherError(order, commonEntity, commonClass, e"The order is invalid: ${'details -> details}")

case class InvalidAsset(theAsset: String)
    extends MatcherError(asset, commonEntity, broken, e"The asset '${'assetId -> theAsset}' is wrong. It should be 'WAVES' or a Base58 string")

case class AssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, commonEntity, blacklisted, e"The asset ${'assetId -> theAsset} is blacklisted")

case class AmountAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, amount, blacklisted, e"The amount asset ${'assetId -> theAsset} is blacklisted")

case class PriceAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, price, blacklisted, e"The price asset ${'assetId -> theAsset} is blacklisted")

case class FeeAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, fee, blacklisted, e"The fee asset ${'assetId -> theAsset} is blacklisted")

case class AddressIsBlacklisted(address: Address)
    extends MatcherError(account, commonEntity, blacklisted, e"The account ${'address -> address} is blacklisted")

case class BalanceNotEnough(required: List[Amount], actual: List[Amount])
    extends MatcherError(
      account,
      balance,
      notEnough,
      e"Not enough tradable balance. The order requires ${'required -> required}, but available are ${'actual -> actual}"
    )

object BalanceNotEnough {
  def apply(required: Map[Asset, Long], actual: Map[Asset, Long])(implicit efc: ErrorFormatterContext): BalanceNotEnough =
    new BalanceNotEnough(mk(required), mk(actual))

  private def mk(input: Map[Asset, Long])(implicit efc: ErrorFormatterContext): List[Amount] =
    input
      .map { case (id, v) => Amount(id, Denormalization.denormalizeAmountAndFee(v, efc.assetDecimals(id))) }
      .toList
      .sortBy(x => AssetPair.assetIdStr(x.asset))
}

case class ActiveOrdersLimitReached(maxActiveOrders: Long)
    extends MatcherError(account, order, limitReached, e"The limit of ${'limit -> maxActiveOrders} active orders has been reached")

case class OrderDuplicate(id: Order.Id) extends MatcherError(account, order, duplicate, e"The order ${'id -> id} has already been placed")

case class OrderNotFound(id: Order.Id) extends MatcherError(order, commonEntity, notFound, e"The order ${'id -> id} not found")

case class OrderCanceled(id: Order.Id) extends MatcherError(order, commonEntity, canceled, e"The order ${'id -> id} is canceled")

case class OrderFull(id: Order.Id) extends MatcherError(order, commonEntity, limitReached, e"The order ${'id -> id} is filled")

case class OrderFinalized(id: Order.Id) extends MatcherError(order, commonEntity, immutable, e"The order ${'id -> id} is finalized")

case class OrderVersionUnsupported(version: Byte, requiredFeature: BlockchainFeature)
    extends MatcherError(
      feature,
      order,
      unsupported,
      e"The order of version ${'version -> version} isn't yet supported, see the activation status of '${'featureName -> requiredFeature}'"
    )

case object RequestInvalidSignature extends MatcherError(request, signature, commonClass, e"The request has an invalid signature")

case class AccountFeatureUnsupported(x: BlockchainFeature)
    extends MatcherError(
      feature,
      account,
      unsupported,
      e"An account's feature isn't yet supported, see the activation status of '${'featureName -> x}'"
    )

case class AccountNotSupportOrderVersion(address: Address, requiredVersion: Byte, givenVersion: Byte)
    extends MatcherError(
      account,
      order,
      unsupported,
      e"The account ${'address -> address} requires the version >= ${'required -> requiredVersion}, but given ${'given -> givenVersion}"
    )

case class AccountScriptReturnedError(address: Address, scriptMessage: String)
    extends MatcherError(account,
                         script,
                         commonClass,
                         e"The account's script of ${'address -> address} returned the error: ${'scriptError -> scriptMessage}")

case class AccountScriptDeniedOrder(address: Address)
    extends MatcherError(account, script, denied, e"The account's script of ${'address -> address} rejected the order")

case class AccountScriptUnexpectResult(address: Address, returnedObject: String)
    extends MatcherError(
      account,
      script,
      unexpected,
      e"""The account's script of ${'address -> address} is broken, please contact with the owner.
                     |The returned object is '${'returnedObject -> returnedObject}'"""
    )

case class AccountScriptException(address: Address, errorName: String, errorText: String)
    extends MatcherError(
      account,
      script,
      broken,
      e"""The account's script of ${'address -> address} is broken, please contact with the owner.
                     |The returned error is ${'errorName -> errorName}, the text is: ${'errorText -> errorText}"""
    )

case class AssetFeatureUnsupported(x: BlockchainFeature, theAsset: IssuedAsset)
    extends MatcherError(
      feature,
      asset,
      unsupported,
      e"""An asset's feature isn't yet supported for '${'assetId -> theAsset}',
                     |see the activation status of '${'featureName -> x.description}'"""
    )

case class AssetScriptReturnedError(theAsset: IssuedAsset, scriptMessage: String)
    extends MatcherError(asset,
                         script,
                         commonClass,
                         e"The asset's script of ${'assetId -> theAsset} returned the error: ${'scriptError -> scriptMessage}")

case class AssetScriptDeniedOrder(theAsset: IssuedAsset)
    extends MatcherError(asset, script, denied, e"The asset's script of ${'assetId -> theAsset} rejected the order")

case class AssetScriptUnexpectResult(theAsset: IssuedAsset, returnedObject: String)
    extends MatcherError(
      asset,
      script,
      unexpected,
      e"""The asset's script of ${'assetId -> theAsset} is broken, please contact with the owner.
                   |The returned object is '${'returnedObject -> returnedObject}'"""
    )

case class AssetScriptException(theAsset: IssuedAsset, errorName: String, errorText: String)
    extends MatcherError(
      asset,
      script,
      broken,
      e"""The asset's script of ${'assetId -> theAsset} is broken, please contact with the owner.
                   |The returned error is ${'errorName -> errorName}, the text is: ${'errorText -> errorText}"""
    )

case class DeviantOrderPrice(orderType: OrderType, orderPrice: Price, deviationSettings: DeviationsSettings)
    extends MatcherError(
      order,
      price,
      outOfBound,
      if (orderType == OrderType.BUY)
        e"""The buy order's price ${'price -> orderPrice} is out of deviation bounds. It should meet the following matcher's requirements:
         |${'bestBidPercent -> (100 - deviationSettings.maxPriceProfit)}% of best bid price <= order price <=
         |${'bestAskPercent -> (100 + deviationSettings.maxPriceLoss)}% of best ask price"""
      else
        e"""The sell order's price ${'price -> orderPrice} is out of deviation bounds. It should meet the following matcher's requirements:
           |${'bestBidPercent -> (100 - deviationSettings.maxPriceLoss)}% of best bid price <= order price <=
           |${'bestAskPercent -> (100 + deviationSettings.maxPriceProfit)}% of best ask price"""
    )
object DeviantOrderPrice {
  def apply(ord: Order, deviationSettings: DeviationsSettings)(implicit efc: ErrorFormatterContext): DeviantOrderPrice =
    DeviantOrderPrice(ord.orderType, Price(ord.assetPair, ord.price), deviationSettings)
}

case class DeviantOrderMatcherFee(orderType: OrderType, matcherFee: Amount, deviationSettings: DeviationsSettings)
    extends MatcherError(
      order,
      fee,
      outOfBound,
      if (orderType == OrderType.BUY)
        e"""The buy order's matcher fee ${'matcherFee -> matcherFee} is out of deviation bounds. It should meet the following matcher's requirements:
       |matcher fee >= ${'bestAskFeePercent -> (100 - deviationSettings.maxFeeDeviation)}% of fee which should be paid in case of matching with best ask"""
      else
        e"""The sell order's matcher fee ${'matcherFee -> matcherFee} is out of deviation bounds. It should meet the following matcher's requirements:
         |matcher fee >= ${'bestBidFeePercent -> (100 - deviationSettings.maxFeeDeviation)}% of fee which should be paid in case of matching with best bid"""
    )
object DeviantOrderMatcherFee {
  def apply(ord: Order, deviationSettings: DeviationsSettings)(implicit efc: ErrorFormatterContext): DeviantOrderMatcherFee =
    DeviantOrderMatcherFee(ord.orderType, Amount(ord.matcherFeeAssetId, ord.matcherFee), deviationSettings)
}

case class AssetPairSameAssets(theAsset: Asset)
    extends MatcherError(
      order,
      assetPair,
      duplicate,
      e"The amount and price assets must be different, but they are: ${'assetId -> theAsset}"
    )

case class AssetPairIsDenied(theAssetPair: AssetPair)
    extends MatcherError(order, assetPair, denied, e"Trading is denied for the ${'assetPair -> theAssetPair} asset pair")

case class OrderAssetPairReversed(theAssetPair: AssetPair)
    extends MatcherError(order, assetPair, unsupported, e"The ${'assetPair -> theAssetPair} asset pair should be reversed")

case class OrderVersionDenied(theVersion: Byte, allowedVersions: Set[Byte])
    extends MatcherError(
      order,
      version,
      denied,
      e"""The orders of version ${'version -> theVersion} are denied by matcher.
                     |Allowed order versions are: ${'allowedOrderVersions -> allowedVersions.toList.sorted}"""
    )

case class UnsupportedOrderVersion(theVersion: Byte)
    extends MatcherError(
      order,
      version,
      unsupported,
      e"""The orders of version ${'version -> theVersion} is not supported.
                     |Supported order versions can be obtained via /matcher/settings GET method"""
    )

case class OrderInvalidAmount(orderAmount: Amount, amtSettings: OrderRestrictionsSettings)
    extends MatcherError(
      order,
      amount,
      denied,
      e"""The order's amount
                     |${'amount -> orderAmount}
                     |does not meet matcher's requirements:
                     |max amount = ${'max -> amtSettings.maxAmount},
                     |min amount = ${'min -> amtSettings.minAmount},
                     |step amount = ${'step -> amtSettings.stepAmount}"""
    )

object OrderInvalidAmount {
  def apply(ord: Order, amtSettings: OrderRestrictionsSettings)(implicit efc: ErrorFormatterContext): OrderInvalidAmount =
    OrderInvalidAmount(Amount(ord.assetPair.amountAsset, ord.amount), amtSettings)
}

case class PriceLastDecimalsMustBeZero(insignificantDecimals: Int)
    extends MatcherError(order, price, unexpected, e"Invalid price, last ${'insignificantDecimals -> insignificantDecimals} digits must be 0")

case class OrderInvalidPrice(orderPrice: Price, prcSettings: OrderRestrictionsSettings)
    extends MatcherError(
      order,
      price,
      denied,
      e"""The order's price
                   |${'price -> orderPrice}
                   |does not meet matcher's requirements:
                   |max price = ${'max -> prcSettings.maxPrice},
                   |min price = ${'min -> prcSettings.minPrice},
                   |step price = ${'step -> prcSettings.stepPrice}"""
    )

object OrderInvalidPrice {
  def apply(ord: Order, prcSettings: OrderRestrictionsSettings)(implicit efc: ErrorFormatterContext): OrderInvalidPrice =
    OrderInvalidPrice(Price(ord.assetPair, ord.price), prcSettings)
}

case class MarketOrderCancel(id: Order.Id)
    extends MatcherError(marketOrder, commonEntity, disabled, e"The market order ${'id -> id} cannot be cancelled manually")

case class InvalidMarketOrderPrice(orderType: OrderType, orderPrice: Price)
    extends MatcherError(
      marketOrder,
      price,
      outOfBound,
      if (orderType == OrderType.BUY)
        e"""Price of the buy market order
         |(${'orderPrice -> orderPrice})
         |is too low for its full execution with the current market state"""
      else
        e"""Price of the sell market order
         |(${'orderPrice -> orderPrice})
         |is too high for its full execution with the current market state"""
    )

object InvalidMarketOrderPrice {
  def apply(mo: Order)(implicit efc: ErrorFormatterContext): InvalidMarketOrderPrice =
    InvalidMarketOrderPrice(mo.orderType, Price(mo.assetPair, mo.price))
}

case class OrderInvalidPriceLevel(orderPrice: Price, minOrderPrice: Price)
    extends MatcherError(
      order,
      price,
      notEnough,
      e"""The buy order's price
       |${'price -> orderPrice}
       |does not meet matcher's requirements:
       |price >= ${'tickSize -> minOrderPrice} (actual tick size).
       |Orders can not be placed into level with price 0"""
    )
object OrderInvalidPriceLevel {
  def apply(ord: Order, tickSize: Long)(implicit efc: ErrorFormatterContext): OrderInvalidPriceLevel =
    OrderInvalidPriceLevel(Price(ord.assetPair, ord.price), Price(ord.assetPair, tickSize))
}

case object WavesNodeConnectionBroken
    extends MatcherError(connectivity, commonEntity, broken, e"Waves Node is unavailable, please retry later or contact with the administrator")

case object UnexpectedError
    extends MatcherError(
      commonEntity,
      commonEntity,
      unexpected,
      e"An unexpected error occurred"
    )

case object WavesImmutableRate extends MatcherError(rate, commonEntity, immutable, e"The rate for ${'assetId -> Waves} cannot be changed")

case object NonPositiveAssetRate extends MatcherError(rate, commonEntity, outOfBound, e"Asset rate should be positive")

case class RateNotFound(theAsset: Asset)
    extends MatcherError(rate, commonEntity, notFound, e"The rate for the asset ${'assetId -> theAsset} was not specified")

case class InvalidJson(fields: List[String])
  extends MatcherError(
    request,
    commonEntity,
    broken,
    if (fields.isEmpty) e"The provided JSON is invalid. Check the documentation"
    else e"The provided JSON contains invalid fields: ${'invalidFields -> fields}. Check the documentation"
  )

case object ApiKeyIsNotProvided
    extends MatcherError(auth, commonEntity, notProvided, e"API key is not provided in the configuration, please contact with the administrator")

case object ApiKeyIsNotValid extends MatcherError(auth, commonEntity, commonClass, e"Provided API key is not correct")

sealed abstract class Entity(val code: Int)
object Entity {
  object common  extends Entity(0)
  object request extends Entity(1)
  object feature extends Entity(2)
  object account extends Entity(3)
  object address extends Entity(4)

  object exchangeTx extends Entity(5)

  object balance extends Entity(6)
  object script  extends Entity(7)

  object orderBook extends Entity(8)
  object order     extends Entity(9)

  object version     extends Entity(10)
  object asset       extends Entity(11)
  object pubKey      extends Entity(12)
  object signature   extends Entity(13)
  object assetPair   extends Entity(14)
  object amount      extends Entity(15)
  object price       extends Entity(16)
  object fee         extends Entity(17)
  object expiration  extends Entity(18)
  object marketOrder extends Entity(19)
  object rate        extends Entity(20)

  object producer     extends Entity(100)
  object connectivity extends Entity(101)
  object auth         extends Entity(102)
}

sealed abstract class Class(val code: Int)
object Class {
  object common       extends Class(0)
  object broken       extends Class(1)
  object denied       extends Class(2)
  object unsupported  extends Class(3)
  object unexpected   extends Class(4)
  object blacklisted  extends Class(5)
  object notEnough    extends Class(6)
  object limitReached extends Class(7)
  object duplicate    extends Class(8)
  object notFound     extends Class(9)
  object canceled     extends Class(10)
  object immutable    extends Class(11)
  object timedOut     extends Class(12)
  object starting     extends Class(13)
  object stopping     extends Class(14)
  object outOfBound   extends Class(15)
  object disabled     extends Class(16)
  object notProvided  extends Class(17)
}
