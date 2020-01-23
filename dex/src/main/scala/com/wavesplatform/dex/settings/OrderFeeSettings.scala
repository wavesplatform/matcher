package com.wavesplatform.dex.settings

import cats.data.Validated.Valid
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.foldable._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{Waves, _}
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.FeeMode.FeeMode
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator.ErrorsListOr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{Reads, Writes}

object OrderFeeSettings {

  sealed trait OrderFeeSettings
  case class DynamicSettings(baseFee: Long)                        extends OrderFeeSettings
  case class FixedSettings(defaultAssetId: Asset, minFee: Long)    extends OrderFeeSettings
  case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  implicit val orderFeeSettingsReader: ValueReader[OrderFeeSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def getPrefixByMode(mode: FeeMode): String = s"$path.$mode"

    def validateDynamicSettings: ErrorsListOr[DynamicSettings] = {
      cfgValidator.validateByPredicate[Long](s"${getPrefixByMode(FeeMode.DYNAMIC)}.base-fee")(
        predicate = fee => 0 < fee,
        errorMsg = s"required 0 < base fee"
      ) map DynamicSettings
    }

    def validateFixedSettings: ErrorsListOr[FixedSettings] = {

      val prefix         = getPrefixByMode(FeeMode.FIXED)
      val feeValidator   = cfgValidator.validateByPredicate[Long](s"$prefix.min-fee") _
      val assetValidated = cfgValidator.validate[Asset](s"$prefix.asset")

      val feeValidated = assetValidated match {
        case Valid(Waves) => feeValidator(fee => 0 < fee, s"required 0 < fee")
        case _            => feeValidator(_ > 0, "required 0 < fee")
      }

      (assetValidated, feeValidated) mapN FixedSettings
    }

    def validatePercentSettings: ErrorsListOr[PercentSettings] = {
      val prefix = getPrefixByMode(FeeMode.PERCENT)
      (
        cfgValidator.validate[AssetType](s"$prefix.asset-type"),
        cfgValidator.validatePercent(s"$prefix.min-fee")
      ) mapN PercentSettings
    }

    def getSettingsByMode(mode: FeeMode): ErrorsListOr[OrderFeeSettings] = mode match {
      case FeeMode.DYNAMIC => validateDynamicSettings
      case FeeMode.FIXED   => validateFixedSettings
      case FeeMode.PERCENT => validatePercentSettings
    }

    cfgValidator.validate[FeeMode](s"$path.mode").toEither flatMap (mode => getSettingsByMode(mode).toEither) match {
      case Left(errorsAcc)         => throw new Exception(errorsAcc.mkString_(", "))
      case Right(orderFeeSettings) => orderFeeSettings
    }
  }
}

object AssetType extends Enumeration {
  type AssetType = Value

  val AMOUNT    = Value("amount")
  val PRICE     = Value("price")
  val SPENDING  = Value("spending")
  val RECEIVING = Value("receiving")

  implicit val assetTypeReads: Reads[AssetType]   = Reads.enumNameReads(AssetType)
  implicit val assetTypeWrites: Writes[AssetType] = Writes.enumNameWrites[AssetType.type]
}

object FeeMode extends Enumeration {
  type FeeMode = Value

  val DYNAMIC = Value("dynamic")
  val FIXED   = Value("fixed")
  val PERCENT = Value("percent")

  implicit val feeModeReads: Reads[FeeMode]   = Reads.enumNameReads(FeeMode)
  implicit val feeModeWrites: Writes[FeeMode] = Writes.enumNameWrites[FeeMode.type]
}
