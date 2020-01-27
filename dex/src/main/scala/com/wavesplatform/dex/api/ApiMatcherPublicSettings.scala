package com.wavesplatform.dex.api

import com.wavesplatform.dex.api.ApiMatcherPublicSettings.OrderFeePublicSettings
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json
import com.wavesplatform.dex.settings.AssetType.AssetType
import play.api.libs.json._

case class ApiMatcherPublicSettings(priceAssets: Seq[Asset], orderFee: OrderFeePublicSettings, orderVersions: Seq[Byte])

object ApiMatcherPublicSettings {
  private implicit val assetDoubleMapFormat: Format[Map[Asset, Double]] = json.assetDoubleMapFormat // IntelliJ IDEA issue

  sealed trait OrderFeePublicSettings extends Product with Serializable
  object OrderFeePublicSettings {
    case class Dynamic(baseFee: Long, rates: Map[Asset, Double]) extends OrderFeePublicSettings
    object Dynamic {
      implicit val dynamicFormat: Format[Dynamic] = Json.format[Dynamic]
    }

    case class Fixed(assetId: Asset, minFee: Long) extends OrderFeePublicSettings
    object Fixed {
      implicit val fixedFormat: Format[Fixed] = Json.format[Fixed]
    }

    case class Percent(`type`: AssetType, minFee: Double) extends OrderFeePublicSettings
    object Percent {
      implicit val percentFormat: Format[Percent] = Json.format[Percent]
    }

    implicit val orderFeePublicSettingsFormat: Format[OrderFeePublicSettings] = Format(
      fjs = Reads { json =>
        val r = (json \ "dynamic").asOpt[Dynamic] orElse (json \ "fixed").asOpt[Fixed] orElse (json \ "percent").asOpt[Percent]
        r.fold[JsResult[OrderFeePublicSettings]](JsError(s"Can't parse as OrderFeePublicSettings: ${Json.stringify(json)}"))(JsSuccess(_))
      },
      tjs = Writes {
        case x: Dynamic => toJson("dynamic", x)(Dynamic.dynamicFormat)
        case x: Fixed   => toJson("fixed", x)(Fixed.fixedFormat)
        case x: Percent => toJson("percent", x)(Percent.percentFormat)
      }
    )

    private def toJson[T](key: String, x: T)(implicit w: Writes[T]): JsObject = Json.obj(key -> w.writes(x))
  }

  implicit val matcherPublicSettingsFormat: OFormat[ApiMatcherPublicSettings] = Json.format[ApiMatcherPublicSettings]
}
