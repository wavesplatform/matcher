package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws._
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error.ErrorFormatterContext
import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.math.{BigDecimal, RoundingMode}

final case class WsMatchTransactionInfo(
  txId: ByteStr,
  timestamp: Long,
  price: Double,
  executedAmountAssets: Double,
  executedPriceAssets: Double
)

object WsMatchTransactionInfo {

  def normalized(assetPair: AssetPair, txId: ByteStr, timestamp: Long, price: Long, executedAmountAssets: Long)(implicit
    efc: ErrorFormatterContext
  ): WsMatchTransactionInfo = {

    val ad = efc.unsafeAssetDecimals(assetPair.amountAsset)
    val pd = efc.unsafeAssetDecimals(assetPair.priceAsset)

    def calcAmountOfPriceAssets(): Long = BigDecimal.valueOf(executedAmountAssets)
      .multiply(BigDecimal.valueOf(price))
      .scaleByPowerOfTen(-Order.PriceConstantExponent)
      .setScale(0, RoundingMode.FLOOR)
      .longValue()

    WsMatchTransactionInfo(
      txId,
      timestamp,
      price = denormalizePrice(price, ad, pd).toDouble,
      executedAmountAssets = denormalizeAmountAndFee(executedAmountAssets, ad).toDouble,
      executedPriceAssets = denormalizeAmountAndFee(calcAmountOfPriceAssets(), pd).toDouble
    )
  }

  implicit val wsMatchTransactionFormat: Format[WsMatchTransactionInfo] =
    (
      (__ \ "i").format[ByteStr] and // id
        (__ \ "t").format[Long] and // timestamp
        (__ \ "p").format[Double](doubleAsStringFormat) and // match execution price
        (__ \ "A").format[Double](doubleAsStringFormat) and // amount asset quantity executed in match
        (__ \ "P").format[Double](doubleAsStringFormat) // price asset quantity executed in match
    )(WsMatchTransactionInfo.apply, unlift(WsMatchTransactionInfo.unapply))

}
