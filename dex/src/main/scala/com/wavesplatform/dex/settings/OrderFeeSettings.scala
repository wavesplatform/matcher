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
import monix.eval.Coeval
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{JsObject, Json}

object OrderFeeSettings {

  sealed trait OrderFeeSettings {

    def getJson(matcherAccountFee: Long, ratesJson: JsObject): Coeval[JsObject] = Coeval.evalOnce {
      Json.obj(
        this match {
          case ds: DynamicSettings =>
            "dynamic" -> Json.obj(
              "baseFee" -> (ds.maxBaseFee + matcherAccountFee),
              "rates"   -> ratesJson
            )
          case FixedSettings(defaultAssetId, minFee) =>
            "fixed" -> Json.obj(
              "assetId" -> defaultAssetId.toString,
              "minFee"  -> minFee
            )
          case PercentSettings(assetType, minFee) =>
            "percent" -> Json.obj(
              "type"   -> assetType,
              "minFee" -> minFee
            )
        }
      )
    }
  }

  final case class DynamicSettings(baseMakerFee: Long, baseTakerFee: Long) extends OrderFeeSettings {
    val maxBaseFee: Long   = math.max(baseMakerFee, baseTakerFee)
    val makerRatio: Double = (BigDecimal(baseMakerFee) / maxBaseFee).toDouble
    val takerRatio: Double = (BigDecimal(baseTakerFee) / maxBaseFee).toDouble
  }

  object DynamicSettings {
    def symmetric(baseFee: Long): DynamicSettings = DynamicSettings(baseFee, baseFee)
  }

  final case class FixedSettings(defaultAsset: Asset, minFee: Long)      extends OrderFeeSettings
  final case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  implicit val orderFeeSettingsReader: ValueReader[OrderFeeSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def getPrefixByMode(mode: FeeMode): String = s"$path.$mode"

    def validateDynamicSettings: ErrorsListOr[DynamicSettings] = {
      val prefix = getPrefixByMode(FeeMode.DYNAMIC)
      (
        cfgValidator.validateByPredicate[Long](s"$prefix.base-maker-fee")(predicate = fee => 0 < fee, errorMsg = s"required 0 < base maker fee"),
        cfgValidator.validateByPredicate[Long](s"$prefix.base-taker-fee")(predicate = fee => 0 < fee, errorMsg = s"required 0 < base taker fee"),
      ) mapN DynamicSettings.apply
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
}

object FeeMode extends Enumeration {
  type FeeMode = Value

  val DYNAMIC = Value("dynamic")
  val FIXED   = Value("fixed")
  val PERCENT = Value("percent")
}
