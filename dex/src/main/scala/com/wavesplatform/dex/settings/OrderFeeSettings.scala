package com.wavesplatform.dex.settings

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.AssetPairQuickValidator
import com.wavesplatform.dex.settings.MatcherSettings.assetPairKeyParser
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils._
import pureconfig.ConfigReader
import pureconfig.generic.auto._
import pureconfig.configurable.genericMapReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto

sealed trait OrderFeeSettings extends Product with Serializable {

  def withDynamicCustomAssets(assets: Set[Asset]): OrderFeeSettings = this

}

object OrderFeeSettings {

  type PartialCustomAssetSettings = AssetPairQuickValidator => CompositeSettings.CustomAssetsSettings

  final case class DynamicSettings(baseMakerFee: Long, baseTakerFee: Long, zeroFeeAccounts: Set[PublicKey] = Set.empty)
      extends OrderFeeSettings {
    val maxBaseFee: Long = math.max(baseMakerFee, baseTakerFee)
    val makerRatio = BigDecimal(baseMakerFee) / maxBaseFee
    val takerRatio = BigDecimal(baseTakerFee) / maxBaseFee

  }

  object DynamicSettings extends ConfigReaders {

    def apply(baseMakerFee: Long, baseTakerFee: Long): DynamicSettings =
      DynamicSettings(baseMakerFee, baseTakerFee, Set.empty)

    def symmetric(baseFee: Long, zeroFeeAccounts: Set[PublicKey]): DynamicSettings =
      DynamicSettings(baseFee, baseFee, zeroFeeAccounts)

    def symmetric(baseFee: Long): DynamicSettings =
      symmetric(baseFee, Set.empty)

    implicit val dynamicConfigReader = semiauto
      .deriveReader[DynamicSettings]
      .validatedField(
        validationOf.field[DynamicSettings, "baseMakerFee"].mk(x => rules.gt0(x.baseMakerFee)),
        validationOf.field[DynamicSettings, "baseTakerFee"].mk(x => rules.gt0(x.baseTakerFee))
      )

  }

  final case class FixedSettings(asset: Asset, minFee: Long) extends OrderFeeSettings

  object FixedSettings extends ConfigReaders {

    implicit val fixedConfigReader = semiauto
      .deriveReader[FixedSettings]
      .validatedField(validationOf.field[FixedSettings, "minFee"].mk(x => rules.gtEq0(x.minFee)))

  }

  final case class PercentSettings(assetType: AssetType, minFee: Double, minFeeInWaves: Long) extends OrderFeeSettings {

    def getFeeAsset(order: Order): Asset =
      getFeeAsset(order.assetPair, order.orderType)

    def getFeeAsset(assetPair: AssetPair, orderType: OrderType): Asset =
      assetType match {
        case AssetType.Amount => assetPair.amountAsset
        case AssetType.Price => assetPair.priceAsset
        case AssetType.Receiving => Order.getReceiveAssetId(assetPair, orderType)
        case AssetType.Spending => Order.getSpendAssetId(assetPair, orderType)
      }

  }

  object PercentSettings {

    implicit val percentConfigReader = semiauto
      .deriveReader[PercentSettings]
      .validatedField(validationOf.field[PercentSettings, "minFeeInWaves"].mk(x => rules.gt0(x.minFeeInWaves)))
      .validatedField(validationOf.field[PercentSettings, "minFee"].mk { x =>
        if (x.minFee > 0 && x.minFee <= 100) none else s"${x.minFee} ∈ (0; 100]".some
      })

  }

  final case class CompositeSettings(
    default: OrderFeeSettings,
    dynamicCustomAssets: Boolean = false,
    customPairs: Map[AssetPair, OrderFeeSettings] = Map.empty,
    customAssets: Option[CompositeSettings.CustomAssetsSettings] = None,
    discount: Option[CompositeSettings.DiscountAssetSettings] = None,
    zeroFeeAccounts: Set[PublicKey] = Set.empty
  ) extends OrderFeeSettings {

    override def withDynamicCustomAssets(assets: Set[Asset]): OrderFeeSettings =
      if (dynamicCustomAssets)
        copy(customAssets = customAssets.map(_.copy(assets = assets)))
      else this

    def getOrderFeeSettings(assetPair: AssetPair): OrderFeeSettings =
      customPairs.get(assetPair).orElse {
        customAssets.flatMap(_.getSettings(assetPair))
      }.getOrElse(default)

    def getAllCustomFeeSettings: Map[AssetPair, OrderFeeSettings] =
      customAssets.fold(Map.empty[AssetPair, OrderFeeSettings])(
        _.settingsMap
      ) ++ customPairs // "customPairs" has higher priority than "customAssets"

  }

  object CompositeSettings extends ConfigReaders {

    final case class CustomAssetsSettings(assets: Set[Asset], settings: OrderFeeSettings, validator: AssetPairQuickValidator) {

      private val customPairs =
        assets.foldLeft(Set.empty[AssetPair]) {
          case (acc, elem) =>
            acc ++ assets.map(asset => AssetPair(asset, elem))
        }.filter(validator)

      val settingsMap: Map[AssetPair, OrderFeeSettings] = customPairs.map(p => (p, settings)).toMap

      def getSettings(assetPair: AssetPair): Option[OrderFeeSettings] =
        settingsMap.get(assetPair)

      override def toString: String = s"CustomAssetsSettings(a=$assets, settings=$settings, customPairs=$customPairs)"

    }

    object CustomAssetsSettings {

      type PartialCustomAssetsSettings = AssetPairQuickValidator => CustomAssetsSettings

      implicit val customAssetsSettingsReader: ConfigReader[PartialCustomAssetsSettings] = ConfigReader.forProduct2[
        PartialCustomAssetsSettings,
        Set[Asset],
        OrderFeeSettings
      ]("assets", "settings") {
        case (assets, settings) =>
          CustomAssetsSettings(assets, settings, _)
      }

    }

    final case class DiscountAssetSettings(asset: Asset, value: BigDecimal)

    object DiscountAssetSettings {

      implicit val discountAssetSettingsConfigReader = semiauto
        .deriveReader[DiscountAssetSettings]
        .validatedField(validationOf.field[DiscountAssetSettings, "value"].mk { x =>
          if (x.value >= 0 && x.value <= 100) none else s"${x.value} ∈ [0; 100]".some
        })

    }

    implicit private val feeSettingsReader: ConfigReader[OrderFeeSettings] = ConfigReader.fromCursor[OrderFeeSettings] { cursor =>
      for {
        objCur <- cursor.asObjectCursor
        modeCur <- objCur.atKey("mode")
        modeStr <- modeCur.asString
        feeSettings <-
          modeStr match {
            case "dynamic" => DynamicSettings.dynamicConfigReader.from(objCur.atKeyOrUndefined("dynamic"))
            case "percent" => PercentSettings.percentConfigReader.from(objCur.atKeyOrUndefined("percent"))
            case "fixed" => FixedSettings.fixedConfigReader.from(objCur.atKeyOrUndefined("fixed"))
            case m =>
              objCur.failed(CannotConvert(objCur.objValue.toString, "OrderFeeSettings", s"unexpected mode type $m"))
          }
      } yield feeSettings
    }

    implicit private val feeSettingsMapReader =
      genericMapReader[AssetPair, OrderFeeSettings](assetPairKeyParser)(feeSettingsReader)

    type PartialCompositeSettings = AssetPairQuickValidator => CompositeSettings

    implicit val partialCompositeConfigReader: ConfigReader[PartialCompositeSettings] = ConfigReader.forProduct6[
      PartialCompositeSettings,
      OrderFeeSettings,
      Option[Boolean],
      Option[Map[AssetPair, OrderFeeSettings]],
      Option[PartialCustomAssetSettings],
      Option[CompositeSettings.DiscountAssetSettings],
      Option[Set[PublicKey]]
    ]("default", "dynamic-custom-assets", "custom-pairs", "custom-assets", "discount", "zero-fee-accounts") {
      case (default, dynamicCustomAssets, custom, customAssets, discount, zeroFeeAccounts) =>
        pairValidator: AssetPairQuickValidator =>
          CompositeSettings(
            default,
            dynamicCustomAssets.getOrElse(false),
            custom.getOrElse(Map.empty),
            customAssets.map(_(pairValidator)),
            discount,
            zeroFeeAccounts.getOrElse(Set.empty)
          )

    }

  }

  implicit val orderFeeHint = new WrappedDescendantHint[OrderFeeSettings]("mode") {
    override protected def fieldValue(name: String): String = name.dropRight("Settings".length).toLowerCase
  }

}
