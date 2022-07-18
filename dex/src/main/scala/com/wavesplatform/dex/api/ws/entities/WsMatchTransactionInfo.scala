package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws._
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.AcceptedOrder
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsMatchTransactionInfo(
  txId: ByteStr,
  timestamp: Long,
  price: Double,
  executedAmountAssets: Double,
  executedPriceAssets: Double,
  isTaker: Boolean
)

object WsMatchTransactionInfo {

  def normalized(assetPair: AssetPair, isTaker: Boolean, txId: ByteStr, timestamp: Long, price: Long, executedAmountAssets: Long)(implicit
    efc: ErrorFormatterContext
  ): WsMatchTransactionInfo = {

    val ad = efc.unsafeAssetDecimals(assetPair.amountAsset)
    val pd = efc.unsafeAssetDecimals(assetPair.priceAsset)

    WsMatchTransactionInfo(
      txId,
      timestamp,
      price = denormalizePrice(price, ad, pd).toDouble,
      executedAmountAssets = denormalizeAmountAndFee(executedAmountAssets, ad).toDouble,
      executedPriceAssets = denormalizeAmountAndFee(AcceptedOrder.calcAmountOfPriceAsset(executedAmountAssets, price), pd).toDouble,
      isTaker
    )
  }

  private val makerTakerFormat: Format[Boolean] = Format(
    {
      case JsString("m") => JsSuccess(false)
      case JsString("t") => JsSuccess(true)
      case _ => JsError("Cannot parse maker taker info")
    },
    {
      case false => JsString("m")
      case true => JsString("t")
    }
  )

  implicit val wsMatchTransactionFormat: Format[WsMatchTransactionInfo] =
    (
      (__ \ "i").format[ByteStr] and // id
        (__ \ "t").format[Long] and // timestamp
        (__ \ "p").format[Double](doubleAsStringFormat) and // match execution price
        (__ \ "A").format[Double](doubleAsStringFormat) and // amount asset quantity executed in match
        (__ \ "P").format[Double](doubleAsStringFormat) and // price asset quantity executed in match
        (__ \ "mt").format[Boolean](makerTakerFormat)
    )(WsMatchTransactionInfo.apply, unlift(WsMatchTransactionInfo.unapply))

}
