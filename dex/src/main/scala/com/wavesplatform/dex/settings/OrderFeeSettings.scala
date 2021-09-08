package com.wavesplatform.dex.settings

import cats.implicits.{catsSyntaxOptionId, none}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils._
import pureconfig.generic.auto._
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

    val empty: DynamicSettings = DynamicSettings(1L, 1L, Set.empty)

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

    val empty: FixedSettings = FixedSettings(Asset.Waves, 0L)

    implicit val fixedConfigReader = semiauto
      .deriveReader[FixedSettings]
      .validatedField(validationOf.field[FixedSettings, "minFee"].mk(x => rules.gtEq0(x.minFee)))

  }

  final case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  object PercentSettings {

    val empty: PercentSettings = PercentSettings(AssetType.Amount, 0d)

    implicit val percentConfigReader = semiauto
      .deriveReader[PercentSettings]
      .validatedField(validationOf.field[PercentSettings, "minFee"].mk { x =>
        if (0 < x.minFee && x.minFee <= 100) none else s"${x.minFee} ∈ (0; 100]".some
      })

  }

  implicit val orderFeeHint = new WrappedDescendantHint[OrderFeeSettings]("mode") {
    override protected def fieldValue(name: String): String = name.dropRight("Settings".length).toLowerCase
  }

}
