package com.wavesplatform.dex.error

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.dex.error.Class.{common => commonClass, _}
import com.wavesplatform.dex.error.Entity.{common => commonEntity, _}
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.settings.{DeviationsSettings, OrderRestrictionsSettings, formatValue}
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair.assetIdStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import play.api.libs.json.{JsObject, Json}

sealed abstract class MatcherError(val code: Int, val message: MatcherErrorMessage) {
  import message._

  def this(obj: Entity, part: Entity, klass: Class, message: MatcherErrorMessage) = this(
    (obj.code << 20) + (part.code << 8) + klass.code, // 32 bits = (12 for object) + (12 for part) + (8 for class)
    message
  )

  def json: JsObject = Json.obj(
    "error"    -> code,
    "message"  -> text,
    "template" -> template,
    "params"   -> params
  )

  override def toString: String = s"${getClass.getCanonicalName}(error=$code, message=$text)"
}

case class MatcherErrorMessage(text: String, template: String, params: JsObject)

case object MatcherIsStarting     extends MatcherError(commonEntity, commonEntity, starting, e"System is starting")
case object MatcherIsStopping     extends MatcherError(commonEntity, commonEntity, stopping, e"System is shutting down")
case object FeatureNotImplemented extends MatcherError(commonEntity, feature, unsupported, e"This feature is not implemented")
case object FeatureDisabled       extends MatcherError(commonEntity, feature, disabled, e"This feature is disabled, contact with the administrator")

case class OrderBookBroken(assetPair: AssetPair)
    extends MatcherError(orderBook,
                         commonEntity,
                         broken,
                         e"The order book for ${'assetPair -> assetPair} is unavailable, please contact with the administrator")

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
      e"The required matcher public key for this DEX is ${'required -> Base58.encode(required)}, but given ${'given -> Base58.encode(given)}"
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
      e"Required one of the following: ${'required -> required.map(assetIdStr).mkString(", ")} as asset fee, but given ${'given -> assetIdStr(given)}"
    )

case class FeeNotEnough(required: Long, given: Long, theAsset: Asset)
    extends MatcherError(
      order,
      fee,
      notEnough,
      e"Required ${'required -> required} ${'assetId -> assetIdStr(theAsset)} as fee for this order, but given ${'given -> given} ${'assetId -> assetIdStr(theAsset)}"
    )

case class AssetNotFound(theAsset: IssuedAsset)
    extends MatcherError(asset, commonEntity, notFound, e"The asset ${'assetId -> assetIdStr(theAsset)} not found")

case class CanNotCreateExchangeTransaction(details: String)
    extends MatcherError(exchangeTx, order, commonClass, e"Can't verify the order by an exchange transaction: ${'details -> details}")

case class WrongExpiration(currentTs: Long, minExpirationOffset: Long, givenExpiration: Long)
    extends MatcherError(
      order,
      expiration,
      notEnough,
      e"The expiration should be at least ${'currentTimestamp -> currentTs} + ${'minExpirationOffset -> minExpirationOffset} = ${'minExpiration -> (currentTs + minExpirationOffset)}, but it is ${'given -> givenExpiration}"
    )

case class OrderCommonValidationFailed(details: String)
    extends MatcherError(order, commonEntity, commonClass, e"The order is invalid: ${'details -> details}")

case class InvalidAsset(theAsset: String)
    extends MatcherError(asset, commonEntity, broken, e"The asset '${'assetId -> theAsset}' is wrong. It should be 'WAVES' or a Base58 string")

case class AssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, commonEntity, blacklisted, e"The asset ${'assetId -> assetIdStr(theAsset)} is blacklisted")

case class AmountAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, amount, blacklisted, e"The amount asset ${'assetId -> assetIdStr(theAsset)} is blacklisted")

case class PriceAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, price, blacklisted, e"The price asset ${'assetId -> assetIdStr(theAsset)} is blacklisted")

case class FeeAssetBlacklisted(theAsset: IssuedAsset)
    extends MatcherError(asset, fee, blacklisted, e"The fee asset ${'assetId -> assetIdStr(theAsset)} is blacklisted")

case class AddressIsBlacklisted(address: Address)
    extends MatcherError(account, commonEntity, blacklisted, e"The account ${'address -> address} is blacklisted")

case class BalanceNotEnough(required: Map[Asset, Long], actual: Map[Asset, Long])
    extends MatcherError(
      account,
      balance,
      notEnough,
      e"Not enough tradable balance. The order requires ${'required -> required}, but only ${'actual -> actual} is available"
    )

case class ActiveOrdersLimitReached(maxActiveOrders: Long)
    extends MatcherError(account, order, limitReached, e"The limit of ${'limit -> maxActiveOrders} active orders has been reached")

case class OrderDuplicate(id: Order.Id) extends MatcherError(account, order, duplicate, e"The order ${'id         -> id} has already been placed")
case class OrderNotFound(id: Order.Id)  extends MatcherError(order, commonEntity, notFound, e"The order ${'id     -> id} not found")
case class OrderCanceled(id: Order.Id)  extends MatcherError(order, commonEntity, canceled, e"The order ${'id     -> id} is canceled")
case class OrderFull(id: Order.Id)      extends MatcherError(order, commonEntity, limitReached, e"The order ${'id -> id} is filled")
case class OrderFinalized(id: Order.Id) extends MatcherError(order, commonEntity, immutable, e"The order ${'id    -> id} is finalized")

case class OrderVersionUnsupported(version: Byte, requiredFeature: BlockchainFeature)
    extends MatcherError(
      feature,
      order,
      unsupported,
      e"The order of version ${'version -> version} isn't yet supported, see the activation status of '${'featureName -> requiredFeature.description}'"
    )

case object RequestInvalidSignature extends MatcherError(request, signature, commonClass, e"The request has an invalid signature")

case class AccountFeatureUnsupported(x: BlockchainFeature)
    extends MatcherError(
      feature,
      account,
      unsupported,
      e"An account's feature isn't yet supported, see the activation status of '${'featureName -> x.description}'"
    )

case class AccountNotSupportOrderVersion(address: Address, requiredVersion: Byte, givenVersion: Byte)
    extends MatcherError(
      account,
      order,
      unsupported,
      e"The account ${'address -> address} requires the version >= ${'requiredVersion -> requiredVersion}, but given ${'givenVersion -> givenVersion}"
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
      e"The account's script of ${'address -> address} is broken, please contact with the owner. The returned object is '${'invalidObject -> returnedObject}'"
    )

case class AccountScriptException(address: Address, errorName: String, errorText: String)
    extends MatcherError(
      account,
      script,
      broken,
      e"The account's script of ${'address -> address} is broken, please contact with the owner. The returned error is ${'errorName -> errorName}, the text is: ${'errorText -> errorText}"
    )

case class AssetFeatureUnsupported(x: BlockchainFeature, theAsset: IssuedAsset)
    extends MatcherError(
      feature,
      asset,
      unsupported,
      e"An asset's feature isn't yet supported for '${'assetId -> assetIdStr(theAsset)}', see the activation status of '${'featureName -> x.description}'"
    )

case class AssetScriptReturnedError(theAsset: IssuedAsset, scriptMessage: String)
    extends MatcherError(asset,
                         script,
                         commonClass,
                         e"The asset's script of ${'assetId -> assetIdStr(theAsset)} returned the error: ${'scriptError -> scriptMessage}")

case class AssetScriptDeniedOrder(theAsset: IssuedAsset)
    extends MatcherError(asset, script, denied, e"The asset's script of ${'assetId -> assetIdStr(theAsset)} rejected the order")

case class AssetScriptUnexpectResult(theAsset: IssuedAsset, returnedObject: String)
    extends MatcherError(
      asset,
      script,
      unexpected,
      e"The asset's script of ${'assetId -> assetIdStr(theAsset)} is broken, please contact with the owner. The returned object is '${'invalidObject -> returnedObject}'"
    )

case class AssetScriptException(theAsset: IssuedAsset, errorName: String, errorText: String)
    extends MatcherError(
      asset,
      script,
      broken,
      e"The asset's script of ${'assetId -> assetIdStr(theAsset)} is broken, please contact with the owner. The returned error is ${'errorName -> errorName}, the text is: ${'errorText -> errorText}"
    )

case class DeviantOrderPrice(ord: Order, deviationSettings: DeviationsSettings)
    extends MatcherError(
      order,
      price,
      outOfBound,
      e"The order's price ${'price -> ord.price} is out of deviation bounds (max-price-deviation-profit: ${'maxPriceDeviationProfit -> deviationSettings.maxPriceProfit}%, max-price-deviation-loss: ${'maxPriceDeviationLoss -> deviationSettings.maxPriceLoss}%, in relation to the best-bid/ask)"
    )

case class DeviantOrderMatcherFee(ord: Order, deviationSettings: DeviationsSettings)
    extends MatcherError(
      order,
      fee,
      outOfBound,
      e"The order's matcher fee ${'matcherFee -> ord.matcherFee} is out of deviation bounds (max-price-deviation-fee: ${'maxPriceDeviationFee -> deviationSettings.maxFeeDeviation}%, in relation to the best-bid/ask)"
    )

case class AssetPairSameAssets(theAsset: Asset)
    extends MatcherError(
      order,
      assetPair,
      duplicate,
      e"The amount and price assets must be different, but they are: ${'assetId -> assetIdStr(theAsset)}"
    )

case class AssetPairIsNotAllowed(orderAssetPair: AssetPair)
    extends MatcherError(
      order,
      assetPair,
      denied,
      e"Trading is not allowed for the pair: ${'amountAssetId -> assetIdStr(orderAssetPair.amountAsset)} - ${'priceAssetId -> assetIdStr(orderAssetPair.priceAsset)}"
    )

case class OrderAssetPairReversed(theAssetPair: AssetPair)
    extends MatcherError(order, assetPair, unsupported, e"The ${'assetPair -> theAssetPair} asset pair should be reversed")

case class OrderVersionIsNotAllowed(theVersion: Byte, allowedVersions: Set[Byte])
    extends MatcherError(
      order,
      version,
      denied,
      e"The orders of version ${'version -> theVersion} are not allowed by matcher. Allowed order versions are: ${'allowedOrderVersions -> allowedVersions.toSeq.sorted
        .mkString(", ")}"
    )

case class UnsupportedOrderVersion(theVersion: Byte)
    extends MatcherError(
      order,
      version,
      unsupported,
      e"The orders of version ${'version -> theVersion} is not supported. Supported order versions can be obtained via /matcher/settings GET method"
    )

case class OrderInvalidAmount(ord: Order, amtSettings: OrderRestrictionsSettings, amountAssetDecimals: Int)
    extends MatcherError(
      order,
      amount,
      denied,
      e"The order's amount (${'assetPair -> ord.assetPair}, ${'amount -> formatValue(Denormalization
        .denormalizeAmountAndFee(ord.amount, amountAssetDecimals))}) does not meet matcher requirements: max amount = ${'maxAmount -> formatValue(
        amtSettings.maxAmount)}, min amount = ${'minAmount                                                                         -> formatValue(amtSettings.minAmount)}, step amount = ${'stepAmount -> formatValue(amtSettings.stepAmount)}"
    )

case class PriceLastDecimalsMustBeZero(insignificantDecimals: Int)
    extends MatcherError(order, price, unexpected, e"Invalid price, last ${'insignificantDecimals -> insignificantDecimals} digits must be 0")

case class OrderInvalidPrice(ord: Order, prcSettings: OrderRestrictionsSettings, amountAssetDecimals: Int, priceAssetDecimals: Int)
    extends MatcherError(
      order,
      price,
      denied,
      e"The order's price (${'assetPair -> ord.assetPair}, ${'price -> formatValue(Denormalization
        .denormalizePrice(ord.price, amountAssetDecimals, priceAssetDecimals))}) does not meet matcher requirements: max price = ${'maxPrice -> formatValue(
        prcSettings.maxPrice)}, min price = ${'minPrice                                                                                      -> formatValue(prcSettings.minPrice)}, step price = ${'stepPrice -> formatValue(prcSettings.stepPrice)}"
    )

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

  object version    extends Entity(10)
  object asset      extends Entity(11)
  object pubKey     extends Entity(12)
  object signature  extends Entity(13)
  object assetPair  extends Entity(14)
  object amount     extends Entity(15)
  object price      extends Entity(16)
  object fee        extends Entity(17)
  object expiration extends Entity(18)

  object producer extends Entity(100)
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
}
