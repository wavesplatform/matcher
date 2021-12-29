package com.wavesplatform.dex.settings

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.settings.MatcherSettings.assetPairKeyParser
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils._
import pureconfig.ConfigReader
import pureconfig.generic.auto._
import pureconfig.configurable.genericMapReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto

sealed trait OrderFeeSettings extends Product with Serializable

object OrderFeeSettings {

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

  final case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  object PercentSettings {

    implicit val percentConfigReader = semiauto
      .deriveReader[PercentSettings]
      .validatedField(validationOf.field[PercentSettings, "minFee"].mk { x =>
        if (0 < x.minFee && x.minFee <= 100) none else s"${x.minFee} ∈ (0; 100]".some
      })

  }

  final case class CompositeSettings(
    custom: Map[AssetPair, OrderFeeSettings],
    default: OrderFeeSettings,
    zeroFeeAccounts: Set[PublicKey] = Set.empty
  ) extends OrderFeeSettings {

    def getOrderFeeSettings(assetPair: AssetPair): OrderFeeSettings =
      custom.getOrElse(assetPair, default)

  }

  object CompositeSettings extends ConfigReaders {

    implicit val compositeConfigReader = ConfigReader.fromCursor[CompositeSettings] { cursor =>
      val feeSettingsReader = ConfigReader.fromCursor[OrderFeeSettings] { cursor =>
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

      for {
        objCur <- cursor.asObjectCursor
        customCur = objCur.withoutKey("default").withoutKey("zero-fee-accounts")
        customRes <- genericMapReader[AssetPair, OrderFeeSettings](assetPairKeyParser)(feeSettingsReader).from(customCur)
        defaultCur <- objCur.atKey("default")
        defaultRes <- ConfigReader[OrderFeeSettings](feeSettingsReader).from(defaultCur)
        zeroFeeAccountsCur = objCur.atKeyOrUndefined("zero-fee-accounts")
        zeroFeeAccountsRes <- ConfigReader[Option[Set[PublicKey]]].from(zeroFeeAccountsCur)
      } yield CompositeSettings(customRes, defaultRes, zeroFeeAccountsRes.getOrElse(Set.empty))
    }

  }

  implicit val orderFeeHint = new WrappedDescendantHint[OrderFeeSettings]("mode") {
    override protected def fieldValue(name: String): String = name.dropRight("Settings".length).toLowerCase
  }

}
