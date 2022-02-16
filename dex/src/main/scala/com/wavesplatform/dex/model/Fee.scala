package com.wavesplatform.dex.model

import com.wavesplatform.dex.caches.OrderFeeSettingsCache
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.AcceptedOrder.partialFee
import com.wavesplatform.dex.model.OrderValidator.multiplyFeeByBigDecimal
import com.wavesplatform.dex.settings.OrderFeeSettings.{CompositeSettings, DynamicSettings, FixedSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, OrderFeeSettings}

import scala.annotation.tailrec

//counter = maker
//submitter = taker
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
        val (buy, sell) = splitByType(s, c)

        val (buyAmt, sellAmt) = assetType match {
          case AssetType.Amount => (
              buy.order.getReceiveAmount _,
              sell.order.getSpendAmount _
            )

          case AssetType.Price => (
              buy.order.getSpendAmount _,
              sell.order.getReceiveAmount _
            )

          case AssetType.Receiving => (
              buy.order.getReceiveAmount _,
              sell.order.getReceiveAmount _
            )

          case AssetType.Spending => (
              buy.order.getSpendAmount _,
              sell.order.getSpendAmount _
            )
        }

        def buySellAmt(buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) =
          buyAmt(buyAmount, buyPrice).explicitGet() -> sellAmt(sellAmount, sellPrice).explicitGet()

        def executedFee(o: AcceptedOrder, amountTotal: Long, amountExecuted: Long) = {
          val fee = o.percentMinFee.getOrElse(o.order.matcherFee)
          val correctedFee = {
            val isTaker = o.order.orderType == s.order.orderType
            if (isTaker && o.isBuyOrder && assetType == AssetType.Spending)
              scaleFeeByPriceDiff(s.price, executedPrice)(fee)
            else
              fee
          }
          partialFee(o.percentConstMinFee.getOrElse(correctedFee).max(correctedFee), amountTotal, amountExecuted)
        }

        val (buyAmountExecuted, sellAmountExecuted) = buySellAmt(executedAmount, executedPrice, executedAmount, executedPrice)
        val (buyAmountTotal, sellAmountTotal) = buySellAmt(buy.order.amount, executedPrice, sell.order.amount, executedPrice)

        val buyExecutedFee = executedFee(buy, buyAmountTotal, buyAmountExecuted)
        val sellExecutedFee = executedFee(sell, sellAmountTotal, sellAmountExecuted)

        withZeroFee(
          if (c.isBuyOrder)
            (buyExecutedFee, sellExecutedFee)
          else
            (sellExecutedFee, buyExecutedFee),
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

  private def scaleFeeByPriceDiff(placementPrice: Long, executionPrice: Long)(fee: Long): Long =
    multiplyFeeByBigDecimal(fee, BigDecimal(executionPrice) / BigDecimal(placementPrice))

  private def splitByType(o1: AcceptedOrder, o2: AcceptedOrder): (AcceptedOrder, AcceptedOrder) = {
    require(o1.order.orderType != o2.order.orderType)
    if (o1.order.orderType == OrderType.BUY) (o1, o2) else (o2, o1)
  }

}
