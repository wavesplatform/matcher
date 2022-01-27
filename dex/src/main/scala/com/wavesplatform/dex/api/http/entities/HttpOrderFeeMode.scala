package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.HttpOrderFeeMode.{FeeModeComposite, FeeModeDynamic, FeeModeFixed, FeeModePercent}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.settings.{AssetType, OrderFeeSettings}
import com.wavesplatform.dex.json.assetPairMapFormat
import com.wavesplatform.dex.settings.OrderFeeSettings.CompositeSettings
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

@ApiModel(
  description = "Order Fee Mode. Can be one of: FeeModeDynamic, FeeModeFixed, FeeModePercent, FeeModeComposite",
  subTypes = Array(
    classOf[FeeModeDynamic],
    classOf[FeeModeFixed],
    classOf[FeeModePercent],
    classOf[FeeModeComposite]
  )
)
sealed trait HttpOrderFeeMode extends Product with Serializable

object HttpOrderFeeMode {

  @ApiModel(description = "Basic mode, fee = base fee * asset rate")
  case class FeeModeDynamic(
    @ApiModelProperty(
      value = "Base fee in Wavelets",
      example = "300000"
    ) baseFee: Long,
    @ApiModelProperty(
      value = "Asset Rates as Map[Base58 encoded Asset ID, Long]",
      dataType = "Map[string,number]"
    ) rates: Map[Asset, Double]
  ) extends HttpOrderFeeMode

  object FeeModeDynamic {
    implicit val dynamicFormat: Format[FeeModeDynamic] = Json.format[FeeModeDynamic]
  }

  @ApiModel(description = "Mode with fixed min fee and fixed asset")
  case class FeeModeFixed(
    @ApiModelProperty(
      value = "Base58 encoded accepted Asset ID",
      dataType = "string",
      example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
    ) assetId: Asset,
    @ApiModelProperty(
      value = "Min fee in min asset fractions",
      dataType = "integer",
      example = "5785"
    ) minFee: Long
  ) extends HttpOrderFeeMode

  object FeeModeFixed {
    implicit val fixedFormat: Format[FeeModeFixed] = Json.format[FeeModeFixed]
  }

  @ApiModel(description = "Mode with min fee in amount/price/spending/receiving asset specified as a percentage")
  case class FeeModePercent(
    @ApiModelProperty(
      value = "Type of percent fee",
      dataType = "string",
      allowableValues = "amount, price, spending, receiving",
      example = "price"
    ) `type`: AssetType,
    @ApiModelProperty(
      value = "Min fee in percents",
      dataType = "number",
      example = "1.5"
    ) minFee: Double,
    @ApiModelProperty(
      value = "Min fee in waves",
      dataType = "number",
      example = "300000"
    ) minFeeInWaves: Long
  ) extends HttpOrderFeeMode

  object FeeModePercent {
    implicit val percentFormat: Format[FeeModePercent] = Json.format[FeeModePercent]
  }

  @ApiModel(description = "Mode with various fee settings for different asset pairs")
  case class FeeModeComposite(
    @ApiModelProperty(value = "Default fee mode for all asset pairs except the custom ones")
    default: HttpOrderFeeMode,
    @ApiModelProperty(value = "Custom fee modes for specific asset pairs")
    custom: Map[AssetPair, HttpOrderFeeMode],
    @ApiModelProperty(value = "Discount asset settings")
    discount: Option[HttpDiscount]
  ) extends HttpOrderFeeMode

  object FeeModeComposite {
    implicit val compositeFormat: Format[FeeModeComposite] = Json.format[FeeModeComposite]
  }

  final case class HttpDiscount(
    asset: Asset,
    value: BigDecimal
  )

  object HttpDiscount {
    implicit val discountFormat: OFormat[HttpDiscount] = Json.format[HttpDiscount]

    def fromDiscountSettings(settings: CompositeSettings.DiscountAssetSettings): HttpDiscount =
      HttpDiscount(settings.asset, settings.value)

  }

  implicit val httpOrderFeeModeFormat: Format[HttpOrderFeeMode] = Format(
    fjs = Reads { json =>
      val r =
        (json \ "dynamic").asOpt[FeeModeDynamic] orElse (json \ "fixed").asOpt[FeeModeFixed] orElse
        (json \ "percent").asOpt[FeeModePercent] orElse (json \ "composite").asOpt[FeeModeComposite]
      r.fold[JsResult[HttpOrderFeeMode]](JsError(s"Can't parse as HttpOrderFeeMode: ${Json.stringify(json)}"))(JsSuccess(_))
    },
    tjs = Writes {
      case x: FeeModeDynamic => toJson("dynamic", x)(FeeModeDynamic.dynamicFormat)
      case x: FeeModeFixed => toJson("fixed", x)(FeeModeFixed.fixedFormat)
      case x: FeeModePercent => toJson("percent", x)(FeeModePercent.percentFormat)
      case x: FeeModeComposite => toJson("composite", x)(FeeModeComposite.compositeFormat)
    }
  )

  private def toJson[T](key: String, x: T)(implicit w: Writes[T]): JsObject = Json.obj(key -> w.writes(x))

  def fromSettings(settings: OrderFeeSettings, matcherAccountFee: Long, allRates: Map[Asset, Double]): HttpOrderFeeMode = settings match {
    case x: OrderFeeSettings.DynamicSettings => FeeModeDynamic(x.maxBaseFee + matcherAccountFee, allRates)
    case OrderFeeSettings.FixedSettings(assetId, minFee) => FeeModeFixed(assetId, minFee)
    case OrderFeeSettings.PercentSettings(assetType, minFee, minFeeInWaves) => FeeModePercent(assetType, minFee, minFeeInWaves)
    case OrderFeeSettings.CompositeSettings(default, custom, discount, _) =>
      FeeModeComposite(
        fromSettings(default, matcherAccountFee, allRates),
        custom.view.mapValues(fromSettings(_, matcherAccountFee, allRates)).toMap,
        discount.map(HttpDiscount.fromDiscountSettings)
      )
  }

}
