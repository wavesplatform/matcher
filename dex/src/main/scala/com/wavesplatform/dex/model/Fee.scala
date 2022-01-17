package com.wavesplatform.dex.model

import com.wavesplatform.dex.caches.OrderFeeSettingsCache
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.AcceptedOrder.partialFee
import com.wavesplatform.dex.model.OrderValidator.multiplyFeeByBigDecimal
import com.wavesplatform.dex.settings.OrderFeeSettings.{CompositeSettings, DynamicSettings, FixedSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, OrderFeeSettings}

import scala.annotation.tailrec

object Fee {

  def getMakerTakerFeeByOffset(ofsc: OrderFeeSettingsCache)(offset: Long)(s: AcceptedOrder, c: LimitOrder): (Long, Long) =
    getMakerTakerFee(ofsc getSettingsForOffset offset)(s, c)

  @tailrec
  def getMakerTakerFee(ofs: => OrderFeeSettings, zeroFeeAccounts: Set[PublicKey] = Set.empty)(s: AcceptedOrder, c: LimitOrder): (Long, Long) = {
    val executedPrice = c.price
    val executedAmount = AcceptedOrder.executedAmount(s, c)

    def absoluteFee(totalCounterFee: Long, totalSubmittedFee: Long): (Long, Long) = {
      val counterExecutedFee = partialFee(totalCounterFee, c.order.amount, executedAmount)
      val submittedExecutedFee = partialFee(totalSubmittedFee, s.order.amount, executedAmount)

      (
        if (c.isFirstMatch && c.order.version >= 3) counterExecutedFee max 1L else counterExecutedFee,
        if (s.isFirstMatch && s.order.version >= 3) submittedExecutedFee max 1L else submittedExecutedFee
      )
    }

    def withZeroFee(counterAndSubmittedFee: (Long, Long), zeroFeeAccounts: Set[PublicKey]) = {
      val (counterFee, submittedFee) = counterAndSubmittedFee
      (
        if (zeroFeeAccounts.contains(c.order.sender)) 0L else counterFee,
        if (zeroFeeAccounts.contains(s.order.sender)) 0L else submittedFee
      )
    }

    ofs match {
      case PercentSettings(assetType, _, _) =>
        val (buy, sell) = Order.splitByType(s.order, c.order)

        val (buyAmt, sellAmt) = assetType match {
          case AssetType.Amount => buy.getReceiveAmount _ -> sell.getSpendAmount _
          case AssetType.Price => buy.getSpendAmount _ -> sell.getReceiveAmount _
          case AssetType.Receiving => buy.getReceiveAmount _ -> sell.getReceiveAmount _
          case AssetType.Spending => buy.getSpendAmount _ -> sell.getSpendAmount _
        }

        def buySellFee(buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) =
          buyAmt(buyAmount, buyPrice).explicitGet() -> sellAmt(sellAmount, sellPrice).explicitGet()

        val (buyAmountExecuted, sellAmountExecuted) = buySellFee(executedAmount, executedPrice, executedAmount, executedPrice)
        val (buyAmountTotal, sellAmountTotal) = buySellFee(buy.amount, executedPrice, sell.amount, executedPrice)

        val buyExecutedFee = partialFee(buy.matcherFee, buyAmountTotal, buyAmountExecuted)
        val sellExecutedFee = partialFee(sell.matcherFee, sellAmountTotal, sellAmountExecuted)

        withZeroFee(
          if (c.isBuyOrder) (buyExecutedFee, sellExecutedFee)
          else (sellExecutedFee, buyExecutedFee),
          zeroFeeAccounts
        )

      case settings @ DynamicSettings(_, _, dsZeroFeeAccounts) =>
        withZeroFee(
          absoluteFee(
            totalCounterFee = multiplyFeeByBigDecimal(c.matcherFee, settings.makerRatio),
            totalSubmittedFee = multiplyFeeByBigDecimal(s.matcherFee, settings.takerRatio)
          ),
          zeroFeeAccounts ++ dsZeroFeeAccounts
        )

      case _: FixedSettings =>
        withZeroFee(
          absoluteFee(
            totalCounterFee = c.matcherFee,
            totalSubmittedFee = s.matcherFee
          ),
          zeroFeeAccounts
        )

      case cs: CompositeSettings =>
        getMakerTakerFee(cs.getOrderFeeSettings(s.order.assetPair), zeroFeeAccounts ++ cs.zeroFeeAccounts)(s, c)
    }
  }

}
