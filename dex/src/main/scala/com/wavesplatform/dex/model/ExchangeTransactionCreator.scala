package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionResult, ExchangeTransactionV2, ExchangeTransactionV3}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import com.wavesplatform.dex.model.ExchangeTransactionCreator._

class ExchangeTransactionCreator(
  matcherPrivateKey: KeyPair,
  exchangeTxBaseFee: Long,
  orderV4StartOffset: Long,
  hasMatcherAccountScript: => Boolean,
  hasAssetScript: IssuedAsset => Boolean,
  shouldPassExecParams: (Option[ValidatedCommandWithMeta.Offset], PublicKey) => Boolean,
  lastProcessedOffset: => Long
)(implicit efc: ErrorFormatterContext) {

  def createTransaction(orderExecutedEvent: OrderExecuted): ExchangeTransactionResult[ExchangeTransaction] = {
    import orderExecutedEvent.{counter, executedAmount, submitted, timestamp}
    val (buy, sell) = Order.splitByType(submitted.order, counter.order)

    val (buyFee, sellFee) =
      if (orderExecutedEvent.submitted.isBuyOrder) (orderExecutedEvent.submittedExecutedFee, orderExecutedEvent.counterExecutedFee)
      else (orderExecutedEvent.counterExecutedFee, orderExecutedEvent.submittedExecutedFee)

    val buyWithExecutionInfo =
      ExecutionParamsInProofs.fillMatchInfoInProofs(
        buy,
        orderExecutedEvent.executedAmount,
        orderExecutedEvent.executedPrice,
        shouldPassExecParams(orderExecutedEvent.commandOffset, buy.sender)
      )
    val sellWithExecutionInfo =
      ExecutionParamsInProofs.fillMatchInfoInProofs(
        sell,
        orderExecutedEvent.executedAmount,
        orderExecutedEvent.executedPrice,
        shouldPassExecParams(orderExecutedEvent.commandOffset, sell.sender)
      )

    // matcher always pays fee to the miners in Waves
    val txFee = minFee(exchangeTxBaseFee, hasMatcherAccountScript, counter.order.assetPair, hasAssetScript) +
      // TODO This will be fixed in NODE 1.2.8+, see NODE-2183
      List(orderExecutedEvent.counter.feeAsset, orderExecutedEvent.submitted.feeAsset)
        .count(_.fold(false)(hasAssetScript)) * OrderValidator.ScriptExtraFee
    val offset = orderExecutedEvent.commandOffset.getOrElse(lastProcessedOffset)
    if (offset < orderV4StartOffset)
      ExchangeTransactionV2
        .mkSigned(
          efc.unsafeAssetDecimals(buy.assetPair.amountAsset),
          efc.unsafeAssetDecimals(buy.assetPair.priceAsset),
          matcherPrivateKey,
          buyWithExecutionInfo,
          sellWithExecutionInfo,
          executedAmount,
          orderExecutedEvent.executedPrice,
          buyFee,
          sellFee,
          txFee,
          timestamp
        )
    else
      ExchangeTransactionV3
        .mkSigned(
          efc.unsafeAssetDecimals(buy.assetPair.amountAsset),
          efc.unsafeAssetDecimals(buy.assetPair.priceAsset),
          matcherPrivateKey,
          buyWithExecutionInfo,
          sellWithExecutionInfo,
          executedAmount,
          orderExecutedEvent.executedPrice,
          buyFee,
          sellFee,
          txFee,
          timestamp
        )
  }

}

object ExchangeTransactionCreator {

  type CreateTransaction = OrderExecuted => ExchangeTransactionResult[ExchangeTransaction]

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
