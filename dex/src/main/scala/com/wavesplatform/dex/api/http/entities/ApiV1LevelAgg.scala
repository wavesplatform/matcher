package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.json
import com.wavesplatform.dex.model.LevelAgg
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, JsArray, _}

case class ApiV1LevelAgg(
    @ApiModelProperty(
      dataType = "string",
      example = "831.87648950"
    ) amount: Double,
    @ApiModelProperty(
      dataType = "string",
      example = "0.00012079"
    ) price: Double
)

object ApiV1LevelAgg {

  implicit val doubleFormat: Format[Double] = json.stringAsDoubleFormat

  implicit val apiV1LevelAggReads: Reads[ApiV1LevelAgg] = Reads {
    case JsArray(value) if value.lengthCompare(2) == 0 => JsSuccess(ApiV1LevelAgg(value(1).as[Double], value(0).as[Double]))
    case x                                             => JsError(s"Cannot parse $x as ApiV1LevelAgg")
  }

  def fromLevelAgg(la: LevelAgg, assetPair: AssetPair)(implicit efc: ErrorFormatterContext): ApiV1LevelAgg = ApiV1LevelAgg(
    Denormalization.denormalizeAmountAndFee(la.amount, efc.assetDecimals(assetPair.amountAsset)).toDouble,
    Denormalization.denormalizePrice(la.price, efc.assetDecimals(assetPair.amountAsset), efc.assetDecimals(assetPair.priceAsset)).toDouble
  )
}
