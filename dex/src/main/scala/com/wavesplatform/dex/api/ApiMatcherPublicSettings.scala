package com.wavesplatform.dex.api

import com.wavesplatform.dex.api.ApiMatcherPublicSettings.ApiOrderFeeSettings
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings
import play.api.libs.json._

case class ApiMatcherPublicSettings(matcherPublicKey: PublicKey,
                                    matcherVersion: String,
                                    priceAssets: Seq[Asset],
                                    orderFee: ApiOrderFeeSettings,
                                    orderVersions: Seq[Byte],
                                    networkByte: Int)

object ApiMatcherPublicSettings {

  sealed trait ApiOrderFeeSettings extends Product with Serializable
  object ApiOrderFeeSettings {
    case class Dynamic(baseFee: Long, rates: Map[Asset, Double]) extends ApiOrderFeeSettings
    object Dynamic {
      implicit val dynamicFormat: Format[Dynamic] = Json.format[Dynamic]
    }

    case class Fixed(assetId: Asset, minFee: Long) extends ApiOrderFeeSettings
    object Fixed {
      implicit val fixedFormat: Format[Fixed] = Json.format[Fixed]
    }

    case class Percent(`type`: AssetType, minFee: Double) extends ApiOrderFeeSettings
    object Percent {
      implicit val percentFormat: Format[Percent] = Json.format[Percent]
    }

    implicit val orderFeePublicSettingsFormat: Format[ApiOrderFeeSettings] = Format(
      fjs = Reads { json =>
        val r = (json \ "dynamic").asOpt[Dynamic] orElse (json \ "fixed").asOpt[Fixed] orElse (json \ "percent").asOpt[Percent]
        r.fold[JsResult[ApiOrderFeeSettings]](JsError(s"Can't parse as OrderFeePublicSettings: ${Json.stringify(json)}"))(JsSuccess(_))
      },
      tjs = Writes {
        case x: Dynamic => toJson("dynamic", x)(Dynamic.dynamicFormat)
        case x: Fixed   => toJson("fixed", x)(Fixed.fixedFormat)
        case x: Percent => toJson("percent", x)(Percent.percentFormat)
      }
    )

    private def toJson[T](key: String, x: T)(implicit w: Writes[T]): JsObject = Json.obj(key -> w.writes(x))

    def fromSettings(settings: OrderFeeSettings, matcherAccountFee: Long, allRates: Map[Asset, Double]): ApiOrderFeeSettings = settings match {
      case x: OrderFeeSettings.DynamicSettings                    => Dynamic(x.maxBaseFee + matcherAccountFee, allRates)
      case OrderFeeSettings.FixedSettings(defaultAssetId, minFee) => Fixed(defaultAssetId, minFee)
      case OrderFeeSettings.PercentSettings(assetType, minFee)    => Percent(assetType, minFee)
    }
  }

  implicit val matcherPublicSettingsFormat: OFormat[ApiMatcherPublicSettings] = Json.format[ApiMatcherPublicSettings]
}
