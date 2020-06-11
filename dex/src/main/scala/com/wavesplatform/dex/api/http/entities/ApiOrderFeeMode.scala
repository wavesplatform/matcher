package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.ApiOrderFeeMode.{FeeModeDynamic, FeeModeFixed, FeeModePercent}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

@ApiModel(
  description = "Order Fee Settings. Can be one of: FeeModeDynamic, FeeModeFixed, FeeModePercent",
  subTypes = Array(
    classOf[FeeModeDynamic],
    classOf[FeeModeFixed],
    classOf[FeeModePercent]
  )
)
sealed trait ApiOrderFeeMode extends Product with Serializable

object ApiOrderFeeMode {

  @ApiModel(description = "Basic mode, fee = base fee * asset rate")
  case class FeeModeDynamic(@ApiModelProperty(
                              value = "Base fee in Wavelets",
                              example = "300000"
                            ) baseFee: Long,
                            @ApiModelProperty(
                              value = "Asset Rates as Map[Base58 encoded Asset ID, Long]",
                              dataType = "Map[string,number]"
                            ) rates: Map[Asset, Double])
      extends ApiOrderFeeMode
  object FeeModeDynamic {
    implicit val dynamicFormat: Format[FeeModeDynamic] = Json.format[FeeModeDynamic]
  }

  @ApiModel(description = "Mode with fixed min fee and fixed asset")
  case class FeeModeFixed(@ApiModelProperty(
                            value = "Base58 encoded accepted Asset ID",
                            dataType = "string",
                            example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
                          ) assetId: Asset,
                          @ApiModelProperty(
                            value = "Min fee in min asset fractions",
                            dataType = "integer",
                            example = "5785"
                          ) minFee: Long)
      extends ApiOrderFeeMode
  object FeeModeFixed {
    implicit val fixedFormat: Format[FeeModeFixed] = Json.format[FeeModeFixed]
  }

  @ApiModel(description = "Mode with min fee in amount/price/spending/receiving asset specified as a percentage")
  case class FeeModePercent(@ApiModelProperty(
                              value = "Type of percent fee",
                              dataType = "string",
                              allowableValues = "amount, price, spending, receiving",
                              example = "price"
                            ) `type`: AssetType,
                            @ApiModelProperty(
                              value = "Min fee in percents",
                              dataType = "number",
                              example = "1.5"
                            ) minFee: Double)
      extends ApiOrderFeeMode
  object FeeModePercent {
    implicit val percentFormat: Format[FeeModePercent] = Json.format[FeeModePercent]
  }

  implicit val orderFeePublicSettingsFormat: Format[ApiOrderFeeMode] = Format(
    fjs = Reads { json =>
      val r = (json \ "dynamic").asOpt[FeeModeDynamic] orElse (json \ "fixed").asOpt[FeeModeFixed] orElse (json \ "percent").asOpt[FeeModePercent]
      r.fold[JsResult[ApiOrderFeeMode]](JsError(s"Can't parse as OrderFeePublicSettings: ${Json.stringify(json)}"))(JsSuccess(_))
    },
    tjs = Writes {
      case x: FeeModeDynamic => toJson("dynamic", x)(FeeModeDynamic.dynamicFormat)
      case x: FeeModeFixed   => toJson("fixed", x)(FeeModeFixed.fixedFormat)
      case x: FeeModePercent => toJson("percent", x)(FeeModePercent.percentFormat)
    }
  )

  private def toJson[T](key: String, x: T)(implicit w: Writes[T]): JsObject = Json.obj(key -> w.writes(x))

  def fromSettings(settings: OrderFeeSettings, matcherAccountFee: Long, allRates: Map[Asset, Double]): ApiOrderFeeMode = settings match {
    case x: OrderFeeSettings.DynamicSettings                    => FeeModeDynamic(x.maxBaseFee + matcherAccountFee, allRates)
    case OrderFeeSettings.FixedSettings(defaultAssetId, minFee) => FeeModeFixed(defaultAssetId, minFee)
    case OrderFeeSettings.PercentSettings(assetType, minFee)    => FeeModePercent(assetType, minFee)
  }
}
