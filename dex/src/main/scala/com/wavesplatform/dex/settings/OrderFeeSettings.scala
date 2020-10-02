package com.wavesplatform.dex.settings

import cats.data.Validated.Valid
import cats.implicits.{catsSyntaxOptionId, none}
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.foldable._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{Waves, _}
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.settings.FeeMode
import com.wavesplatform.dex.settings.utils.ConfigCursorsOps.ConfigCursorOps
import com.wavesplatform.dex.settings.utils.{ConfigCursorsOps, ConfigReaders, ConfigSettingsValidator, WrappedDescendantHint, rules, validationOf}
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator.ErrorsListOr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{Reads, Writes}
import pureconfig.ConfigReader
import pureconfig.generic.auto._
import cats.syntax.either._
import com.wavesplatform.dex.settings.AssetType.lowerCaseNamesToValuesMap
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import enumeratum._
import pureconfig.generic.semiauto

sealed trait OrderFeeSettings extends Product with Serializable

object OrderFeeSettings extends ConfigCursorsOps {

  final case class DynamicSettings(baseMakerFee: Long, baseTakerFee: Long) extends OrderFeeSettings {
    val maxBaseFee: Long   = math.max(baseMakerFee, baseTakerFee)
    val makerRatio: Double = (BigDecimal(baseMakerFee) / maxBaseFee).toDouble
    val takerRatio: Double = (BigDecimal(baseTakerFee) / maxBaseFee).toDouble
  }

  object DynamicSettings {
    def symmetric(baseFee: Long): DynamicSettings = DynamicSettings(baseFee, baseFee)

    implicit val dynamicConfigReader = semiauto
      .deriveReader[DynamicSettings]
      .validated(
        validationOf[DynamicSettings, "baseMakerFee"].mk(x => rules.gt0(x.baseMakerFee)),
        validationOf[DynamicSettings, "baseTakerFee"].mk(x => rules.gt0(x.baseTakerFee))
      )
  }

  // defaultAsset
  final case class FixedSettings(asset: Asset, minFee: Long) extends OrderFeeSettings
  object FixedSettings extends ConfigReaders {
    implicit val fixedConfigReader = semiauto
      .deriveReader[FixedSettings]
      .validated(validationOf[FixedSettings, "minFee"].mk(x => rules.gtEq0(x.minFee)))
  }

  final case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings
  object PercentSettings {
    implicit val percentConfigReader = semiauto
      .deriveReader[PercentSettings]
      .validated(validationOf[PercentSettings, "minFee"].mk { x =>
        if (0 < x.minFee && x.minFee <= 100) none else s"${x.minFee} ∈ (0; 100]".some
      })
  }

  implicit val orderFeeHint = new WrappedDescendantHint[OrderFeeSettings]("mode") {
    override protected def fieldValue(name: String): String = name.dropRight("Settings".length).toLowerCase
  }

  // ===

  implicit val orderFeeSettingsReader: ValueReader[OrderFeeSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def getPrefixByMode(mode: FeeMode): String = s"$path.$mode"

    def validateDynamicSettings: ErrorsListOr[DynamicSettings] = {
      val prefix = getPrefixByMode(FeeMode.Dynamic)
      (
        cfgValidator.validateByPredicate[Long](s"$prefix.base-maker-fee")(predicate = fee => 0 < fee, errorMsg = s"required 0 < base maker fee"),
        cfgValidator.validateByPredicate[Long](s"$prefix.base-taker-fee")(predicate = fee => 0 < fee, errorMsg = s"required 0 < base taker fee")
      ) mapN DynamicSettings.apply
    }

    def validateFixedSettings: ErrorsListOr[FixedSettings] = {

      val prefix         = getPrefixByMode(FeeMode.Fixed)
      val feeValidator   = cfgValidator.validateByPredicate[Long](s"$prefix.min-fee") _
      val assetValidated = cfgValidator.validate[Asset](s"$prefix.asset")

      val feeValidated = assetValidated match {
        case Valid(Waves) => feeValidator(fee => 0 < fee, s"required 0 < fee")
        case _            => feeValidator(_ > 0, "required 0 < fee")
      }

      (assetValidated, feeValidated) mapN FixedSettings.apply
    }

    def validatePercentSettings: ErrorsListOr[PercentSettings] = {
      val prefix = getPrefixByMode(FeeMode.Percent)
      (
        cfgValidator.validate[AssetType](s"$prefix.asset-type"),
        cfgValidator.validatePercent(s"$prefix.min-fee")
      ) mapN PercentSettings.apply
    }

    def getSettingsByMode(mode: FeeMode): ErrorsListOr[OrderFeeSettings] = mode match {
      case FeeMode.Dynamic => validateDynamicSettings
      case FeeMode.Fixed   => validateFixedSettings
      case FeeMode.Percent => validatePercentSettings
    }

    cfgValidator.validate[FeeMode](s"$path.mode").toEither flatMap (mode => getSettingsByMode(mode).toEither) match {
      case Left(errorsAcc)         => throw new Exception(errorsAcc.mkString_(", "))
      case Right(orderFeeSettings) => orderFeeSettings
    }
  }

}

// ====

sealed trait AssetType extends EnumEntry
object AssetType extends Enum[AssetType] with PlayLowercaseJsonEnum[AssetType] {
  override val values = findValues

  case object Amount    extends AssetType
  case object Price     extends AssetType
  case object Spending  extends AssetType
  case object Receiving extends AssetType

  implicit val assetTypeConfigReader               = ConfigReader.fromStringOpt(lowerCaseNamesToValuesMap.get)
  implicit def valueReader: ValueReader[AssetType] = ??? // TODO REMOVE
}

// TODO remove
sealed trait FeeMode extends EnumEntry
object FeeMode extends Enum[FeeMode] with PlayLowercaseJsonEnum[FeeMode] {
  override val values = findValues

  case object Dynamic extends FeeMode
  case object Fixed   extends FeeMode
  case object Percent extends FeeMode

  implicit val feeModeConfigReader               = ConfigReader.fromStringOpt(lowerCaseNamesToValuesMap.get)
  implicit def valueReader: ValueReader[FeeMode] = ??? // TODO REMOVE
}
