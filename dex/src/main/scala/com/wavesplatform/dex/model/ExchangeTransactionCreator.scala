package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.ActivationError
import com.wavesplatform.dex.domain.order.{Order, OrderV1}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.model.ExchangeTransactionCreator._
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{OrderFeeSettings, PercentSettings}

import scala.concurrent.ExecutionContext

class ExchangeTransactionCreator(matcherPrivateKey: KeyPair,
                                 exchangeTxBaseFee: Long,
                                 currentOrderFeeSettings: => OrderFeeSettings,
                                 hasMatcherAccountScript: Boolean,
                                 hasAssetScript: IssuedAsset => Boolean)(implicit ec: ExecutionContext) {

  def createTransaction(orderExecutedEvent: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {

    import orderExecutedEvent._

    val price       = counter.price
    val (buy, sell) = Order.splitByType(submitted.order, counter.order)

    def calculateMatcherFee: (Long, Long) = {

      import AcceptedOrder.partialFee

      def isSubmitted[A](order: Order, s: A, c: A): A = if (order.orderType == submitted.order.orderType) s else c
      def executedFee(order: Order): Long             = isSubmitted(order, submittedExecutedFee, counterExecutedFee)
      def isFirstMatch(order: Order): Boolean         = isSubmitted(order, submitted.amount == submitted.order.amount, counter.amount == counter.order.amount)

      def getActualBuySellAmounts(assetType: AssetType, buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) = {

        val (buyAmt, sellAmt) = assetType match {
          case AssetType.AMOUNT    => buy.getReceiveAmount _ -> sell.getSpendAmount _
          case AssetType.PRICE     => buy.getSpendAmount _   -> sell.getReceiveAmount _
          case AssetType.RECEIVING => buy.getReceiveAmount _ -> sell.getReceiveAmount _
          case AssetType.SPENDING  => buy.getSpendAmount _   -> sell.getSpendAmount _
        }

        buyAmt(buyAmount, buyPrice).explicitGet() -> sellAmt(sellAmount, sellPrice).explicitGet()
      }

      currentOrderFeeSettings match {
        case PercentSettings(assetType, _) =>
          val (buyAmountExecuted, sellAmountExecuted) = getActualBuySellAmounts(assetType, executedAmount, price, executedAmount, price)
          val (buyAmountTotal, sellAmountTotal)       = getActualBuySellAmounts(assetType, buy.amount, buy.price, sell.amount, sell.price)

          (
            partialFee(buy.matcherFee, buyAmountTotal, buyAmountExecuted) min buy.matcherFee,
            partialFee(sell.matcherFee, sellAmountTotal, sellAmountExecuted) min sell.matcherFee
          )

        case _ =>
          val (buyExecutedFee, sellExecutedFee)         = executedFee(buy)  -> executedFee(sell)
          val (isFirstMatchForBuy, isFirstMatchForSell) = isFirstMatch(buy) -> isFirstMatch(sell)

          (
            if (isFirstMatchForBuy && submitted.order.version >= 3) buyExecutedFee max 1L else buyExecutedFee,
            if (isFirstMatchForSell && submitted.order.version >= 3) sellExecutedFee max 1L else sellExecutedFee
          )
      }
    }

    val (buyFee, sellFee) = calculateMatcherFee

    // matcher always pays fee to the miners in Waves
    val txFee = minFee(exchangeTxBaseFee, hasMatcherAccountScript, counter.order.assetPair, hasAssetScript)

    if (buy.version >= 2 || sell.version >= 2) {
      ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
    } else
      for {
        v1Buy  <- toV1(buy)
        v1Sell <- toV1(sell)
        tx     <- ExchangeTransactionV1.create(matcherPrivateKey, v1Buy, v1Sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
      } yield tx
  }

  private def toV1(order: Order): Either[ValidationError, OrderV1] = order match {
    case x: OrderV1 => Right(x)
    case _          => Left(ActivationError("Smart Account Trading feature has not been activated yet"))
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
