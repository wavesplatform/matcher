package com.wavesplatform.dex.api.ws.converters

import com.wavesplatform.dex.api.ws.entities.WsMatchTransactionInfo
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.AcceptedOrder

object WsMatchTxInfoConverter {

  def toWs(assetPair: AssetPair, txId: ByteStr, timestamp: Long, price: Long, executedAmountAssets: Long)(implicit
    efc: ErrorFormatterContext
  ): WsMatchTransactionInfo = {

    val ad = efc.unsafeAssetDecimals(assetPair.amountAsset)
    val pd = efc.unsafeAssetDecimals(assetPair.priceAsset)

    WsMatchTransactionInfo(
      txId,
      timestamp,
      price = denormalizePrice(price, ad, pd).toDouble,
      executedAmountAssets = denormalizeAmountAndFee(executedAmountAssets, ad).toDouble,
      executedPriceAssets = denormalizeAmountAndFee(AcceptedOrder.calcAmountOfPriceAsset(executedAmountAssets, price), pd).toDouble
    )
  }

}
