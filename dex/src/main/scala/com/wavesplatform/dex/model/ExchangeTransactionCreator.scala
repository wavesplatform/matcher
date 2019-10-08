package com.wavesplatform.dex.model

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.model.ExchangeTransactionCreator._
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.PercentSettings
import com.wavesplatform.dex.settings.{AssetType, MatcherSettings}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets.exchange._

import scala.concurrent.{ExecutionContext, Future}

class ExchangeTransactionCreator(matcherPrivateKey: KeyPair,
                                 matcherSettings: MatcherSettings,
                                 hasMatcherAccountScript: => Future[Boolean],
                                 hasAssetScript: IssuedAsset => Future[Boolean],
                                 isFeatureActivated: Short => Future[Boolean])(implicit ec: ExecutionContext) {

  private def calculateMatcherFee(buy: Order, sell: Order, executedAmount: Long, executedPrice: Long): (Long, Long) = {

    def calcFee(orderFee: Long, executedAmount: Long, totalAmount: Long): Long = {
      (BigInt(executedAmount) * orderFee / totalAmount).toLong
    }

    def getActualBuySellAmounts(assetType: AssetType, buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) = {

      val (buyAmt, sellAmt) = assetType match {
        case AssetType.AMOUNT    => buy.getReceiveAmount _ -> sell.getSpendAmount _
        case AssetType.PRICE     => buy.getSpendAmount _   -> sell.getReceiveAmount _
        case AssetType.RECEIVING => buy.getReceiveAmount _ -> sell.getReceiveAmount _
        case AssetType.SPENDING  => buy.getSpendAmount _   -> sell.getSpendAmount _
      }

      buyAmt(buyAmount, buyPrice).explicitGet() -> sellAmt(sellAmount, sellPrice).explicitGet()
    }

    matcherSettings.orderFee match {
      case PercentSettings(assetType, _) =>
        val (buyAmountExecuted, sellAmountExecuted) = getActualBuySellAmounts(assetType, executedAmount, executedPrice, executedAmount, executedPrice)
        val (buyAmountTotal, sellAmountTotal)       = getActualBuySellAmounts(assetType, buy.amount, buy.price, sell.amount, sell.price)

        (
          Math.min(buy.matcherFee, calcFee(buy.matcherFee, buyAmountExecuted, buyAmountTotal)),
          Math.min(sell.matcherFee, calcFee(sell.matcherFee, sellAmountExecuted, sellAmountTotal))
        )

      case _ => calcFee(buy.matcherFee, executedAmount, buy.amount) -> calcFee(sell.matcherFee, executedAmount, sell.amount)
    }
  }

  def createTransaction(submitted: AcceptedOrder, counter: LimitOrder, timestamp: Long): Future[Either[ValidationError, ExchangeTransaction]] = {

    val executedAmount    = AcceptedOrder.executedAmount(submitted, counter)
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, executedAmount, price)

    for {
      txFee                          <- minFee(matcherSettings.exchangeTxBaseFee, hasMatcherAccountScript, counter.order.assetPair, hasAssetScript)
      isSmartAccountTradingActivated <- isFeatureActivated(BlockchainFeatures.SmartAccountTrading.id)
    } yield {
      if (isSmartAccountTradingActivated)
        ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
      else
        for {
          v1Buy  <- toV1(buy)
          v1Sell <- toV1(sell)
          tx     <- ExchangeTransactionV1.create(matcherPrivateKey, v1Buy, v1Sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
        } yield tx
    }
  }

  private def toV1(order: Order): Either[ValidationError, OrderV1] = order match {
    case x: OrderV1 => Right(x)
    case _          => Left(ActivationError("Smart Account Trading feature has not been activated yet"))
  }
}

object ExchangeTransactionCreator {

  type CreateTransaction = (AcceptedOrder, LimitOrder, Long) => Future[Either[ValidationError, ExchangeTransaction]]

  /**
    * This function is used for the following purposes:
    *
    *   1. Calculate transaction fee that matcher pays to issue Exchange transaction (ExchangeTransactionCreator, base fee = matcherSettings.exchangeTxBaseFee)
    *   2. Calculate matcher fee that client pays for the order placement and covering matcher expenses (OrderValidator blockchain aware, base fee depends on order fee settings)
    *
    * @see [[com.wavesplatform.transaction.smart.Verifier#verifyExchange verifyExchange]]
    */
  def minFee(baseFee: Long, hasMatcherAccountScript: Future[Boolean], assetPair: AssetPair, hasAssetScript: IssuedAsset => Future[Boolean])(
      implicit ec: ExecutionContext): Future[Long] = {

    def assetFee(assetId: Asset): Future[Long] = assetId.fold { Future.successful(0L) } { issuedAsset =>
      hasAssetScript(issuedAsset).map(getAdditionalFee)
    }

    for {
      hasMatcherScript <- hasMatcherAccountScript
      amountAssetFee   <- assetFee(assetPair.amountAsset)
      priceAssetFee    <- assetFee(assetPair.priceAsset)
    } yield {
      baseFee +
        minAccountFee(hasMatcherScript) +
        amountAssetFee +
        priceAssetFee
    }
  }

  def getAdditionalFee(hasScript: Boolean): Long            = if (hasScript) FeeValidation.ScriptExtraFee else 0L
  def minAccountFee(hasMatcherAccountScript: Boolean): Long = getAdditionalFee(hasMatcherAccountScript)
}
