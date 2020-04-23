package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.model.ExchangeTransactionCreator._

import scala.concurrent.ExecutionContext

class ExchangeTransactionCreator(matcherPrivateKey: KeyPair,
                                 exchangeTxBaseFee: Long,
                                 hasMatcherAccountScript: Boolean,
                                 hasAssetScript: IssuedAsset => Boolean)(implicit ec: ExecutionContext) {

  def createTransaction(orderExecutedEvent: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import orderExecutedEvent.{counter, executedAmount, submitted, timestamp}
    val (buy, sell) = Order.splitByType(submitted.order, counter.order)

    val (buyFee, sellFee) =
      if (orderExecutedEvent.submitted.isBuyOrder) (orderExecutedEvent.submittedExecutedFee, orderExecutedEvent.counterExecutedFee)
      else (orderExecutedEvent.counterExecutedFee, orderExecutedEvent.submittedExecutedFee)

    // matcher always pays fee to the miners in Waves
    val txFee = minFee(exchangeTxBaseFee, hasMatcherAccountScript, counter.order.assetPair, hasAssetScript)
    ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, executedAmount, orderExecutedEvent.executedPrice, buyFee, sellFee, txFee, timestamp)
  }
}

object ExchangeTransactionCreator {

  type CreateTransaction = OrderExecuted => Either[ValidationError, ExchangeTransaction]

  /**
    * This function is used for the following purposes:
    *
    *   1. Calculate matcher fee that CLIENT PAYS TO MATCHER for the order placement and covering matcher expenses (OrderValidator blockchainAware, base fee depends on order fee settings)
    *   2. Calculate transaction fee that MATCHER PAYS TO THE MINERS for issuing Exchange transaction (ExchangeTransactionCreator, base fee = matcherSettings.exchangeTxBaseFee)
    */
  def minFee(baseFee: Long, hasMatcherAccountScript: Boolean, assetPair: AssetPair, hasAssetScript: IssuedAsset => Boolean): Long = {

    def assetFee(assetId: Asset): Long = assetId.fold(0L)(hasAssetScript andThen getAdditionalFeeForScript)

    baseFee +
      getAdditionalFeeForScript(hasMatcherAccountScript) +
      assetFee(assetPair.amountAsset) +
      assetFee(assetPair.priceAsset)
  }

  def getAdditionalFeeForScript(hasScript: Boolean): Long = if (hasScript) OrderValidator.ScriptExtraFee else 0L
}
