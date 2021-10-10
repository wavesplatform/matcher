package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.{Order, OrderV3}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransactionResult, ExchangeTransactionV2}
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.model.ExchangeTransactionCreator._

import java.io.ByteArrayOutputStream

class ExchangeTransactionCreator(
  matcherPrivateKey: KeyPair,
  exchangeTxBaseFee: Long,
  hasMatcherAccountScript: => Boolean,
  hasAssetScript: IssuedAsset => Boolean
) {

  def createTransaction(orderExecutedEvent: OrderExecuted): ExchangeTransactionResult[ExchangeTransactionV2] = {
    import orderExecutedEvent.{counter, executedAmount, submitted, timestamp}
    val (buy, sell) = Order.splitByType(submitted.order, counter.order)

    val (buyFee, sellFee) =
      if (orderExecutedEvent.submitted.isBuyOrder) (orderExecutedEvent.submittedExecutedFee, orderExecutedEvent.counterExecutedFee)
      else (orderExecutedEvent.counterExecutedFee, orderExecutedEvent.submittedExecutedFee)

    // HACK for LP
    val buyWithExecutionInfo = fillMatchInfoInProofs(buy, orderExecutedEvent.executedAmount, orderExecutedEvent.executedPrice, isLp = buyFee == 0)
    val sellWithExecutionInfo = fillMatchInfoInProofs(sell, orderExecutedEvent.executedAmount, orderExecutedEvent.executedPrice, isLp = sellFee == 0)
    // end HACK for LP

    // matcher always pays fee to the miners in Waves
    val txFee = minFee(exchangeTxBaseFee, hasMatcherAccountScript, counter.order.assetPair, hasAssetScript) +
      // TODO This will be fixed in NODE 1.2.8+, see NODE-2183
      List(orderExecutedEvent.counter.feeAsset, orderExecutedEvent.submitted.feeAsset)
        .count(_.fold(false)(hasAssetScript)) * OrderValidator.ScriptExtraFee
    ExchangeTransactionV2.create(
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

  type CreateTransaction = OrderExecuted => ExchangeTransactionResult[ExchangeTransactionV2]

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

  // HACK for LP
  // TODO move to another place

  // see com.wavesplatform.lang.utils.Serialize.ByteArrayOutputStreamOps
  private def encodeToBytes(n: Long, byteCount: Int): ByteStr = {
    val s = new ByteArrayOutputStream(byteCount)
    (byteCount - 1 to 0 by -1).foreach { i =>
      s.write((n >> (8 * i) & 0xffL).toInt)
    }
    ByteStr(s.toByteArray)
  }

  def updateProofs(proofs: Proofs, executedAmount: Long, executedPrice: Long): Proofs =
    proofs.proofs ++ List(encodeToBytes(executedAmount, 8), encodeToBytes(executedPrice, 8))

  def fillMatchInfoInProofs(order: Order, executedAmount: Long, executedPrice: Long): Order =
    order match {
      case order: OrderV3 => order.copy(proofs = updateProofs(order.proofs, executedAmount, executedPrice))
      case _ => order // HACK
    }

  def fillMatchInfoInProofs(order: Order, executedAmount: Long, executedPrice: Long, isLp: Boolean): Order = if (isLp) fillMatchInfoInProofs(order, executedAmount, executedPrice) else order
  // end HACK for LP
}
