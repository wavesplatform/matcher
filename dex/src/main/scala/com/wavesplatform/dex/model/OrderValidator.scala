package com.wavesplatform.dex.model

import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.cache.RateCache
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error._
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainCachingClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderValidator.FutureResult
import com.wavesplatform.dex.settings.OrderFeeSettings._
import com.wavesplatform.dex.settings.{AssetType, DeviationsSettings, MatcherSettings, OrderRestrictionsSettings}
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon

import scala.Either.cond
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode

object OrderValidator extends ScorexLogging {

  type Result[T]       = Either[MatcherError, T]
  type FutureResult[T] = EitherT[Future, MatcherError, T]

  private val timer = Kamon.timer("matcher.validation").refine("type" -> "blockchain")

  val MinExpiration: Long  = 60 * 1000L
  val MaxActiveOrders: Int = 200

  val exchangeTransactionCreationFee: Long = FeeValidation.FeeConstants(ExchangeTransaction.typeId) * FeeValidation.FeeUnit

  private[dex] def multiplyAmountByDouble(a: Long, d: Double): Long = (BigDecimal(a) * d).setScale(0, RoundingMode.HALF_UP).toLong
  private[dex] def multiplyPriceByDouble(p: Long, d: Double): Long  = (BigDecimal(p) * d).setScale(0, RoundingMode.HALF_UP).toLong
  private[dex] def multiplyFeeByDouble(f: Long, d: Double): Long    = (BigDecimal(f) * d).setScale(0, RoundingMode.CEILING).toLong

  private def verifySignature(order: Order): FutureResult[Unit] = liftAsync {
    Verifier
      .verifyAsEllipticCurveSignature(order)
      .bimap(
        e => error.OrderInvalidSignature(order.id(), e.toString),
        _ => Unit
      )
  }

//  private def verifyOrderByAccountScript(blockchain: WavesBlockchainClient, address: Address, order: Order): Result[Unit] =
//    if (blockchain.hasScript(address)) {
//      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading.id))
//        error.AccountFeatureUnsupported(BlockchainFeatures.SmartAccountTrading).asLeft
//      else if (order.version <= 1) error.AccountNotSupportOrderVersion(address, 2, order.version).asLeft
//      else
//        blockchain.runScript(address, order) match {
//          case RunScriptResult.ScriptError(execError)   => error.AccountScriptReturnedError(address, execError).asLeft
//          case RunScriptResult.Denied                   => error.AccountScriptDeniedOrder(address).asLeft
//          case RunScriptResult.Allowed                  => success
//          case RunScriptResult.UnexpectedResult(x)      => error.AccountScriptUnexpectResult(address, x).asLeft
//          case RunScriptResult.Exception(name, message) => error.AccountScriptException(address, name, message).asLeft
//        }
//    } else verifySignature(order)

//  private def verifyOrderByAccountScript(blockchain: WavesBlockchainCachingClient, address: Address, order: Order)(
//      implicit ec: ExecutionContext): FutureResult[Unit] = {
//    EitherT.right[MatcherError] { blockchain.hasScript(address) }.flatMap { addressHasScript =>
//      if (addressHasScript) {
//        EitherT.right[MatcherError] { blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading.id) }.flatMap {
//          ifSmartAccountTradindActivated =>
//            if (ifSmartAccountTradindActivated)
//              liftErrorAsync[Unit] { error.AccountFeatureUnsupported(BlockchainFeatures.SmartAccountTrading) } else if (order.version <= 1)
//              liftErrorAsync[Unit] { error.AccountNotSupportOrderVersion(address, 2, order.version) } else {
//              EitherT.right[MatcherError] { blockchain.runScript(address, order) }.flatMap { runScriptResult =>
//                liftAsync {
//                  runScriptResult match {
//                    case RunScriptResult.ScriptError(execError)   => error.AccountScriptReturnedError(address, execError).asLeft
//                    case RunScriptResult.Denied                   => error.AccountScriptDeniedOrder(address).asLeft
//                    case RunScriptResult.Allowed                  => success
//                    case RunScriptResult.UnexpectedResult(x)      => error.AccountScriptUnexpectResult(address, x).asLeft
//                    case RunScriptResult.Exception(name, message) => error.AccountScriptException(address, name, message).asLeft
//                  }
//                }
//              }
//            }
//        }
//      } else verifySignature(order)
//    }
//  }

  private def verifyOrderByAccountScript(blockchain: WavesBlockchainCachingClient, address: Address, order: Order)(
      implicit ec: ExecutionContext): FutureResult[Unit] = {

    lazy val verifyAddressScript: FutureResult[Unit] = {

      lazy val verifyScript: FutureResult[Unit] = {
        if (order.version <= 1) liftErrorAsync[Unit] { error.AccountNotSupportOrderVersion(address, 2, order.version) } else {
          liftFutureAsync { blockchain.runScript(address, order) } map {
            case RunScriptResult.ScriptError(execError)   => error.AccountScriptReturnedError(address, execError).asLeft
            case RunScriptResult.Denied                   => error.AccountScriptDeniedOrder(address).asLeft
            case RunScriptResult.Allowed                  => success
            case RunScriptResult.UnexpectedResult(x)      => error.AccountScriptUnexpectResult(address, x).asLeft
            case RunScriptResult.Exception(name, message) => error.AccountScriptException(address, name, message).asLeft
          }
        }
      }

      liftFutureAsync { blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading.id) }
        .ifM(verifyScript, liftErrorAsync[Unit] { error.AccountFeatureUnsupported(BlockchainFeatures.SmartAccountTrading) })
    }

    liftFutureAsync { blockchain.hasScript(address) } ifM (verifyAddressScript, verifySignature(order))
  }

//  private def verifySmartToken(blockchain: WavesBlockchainClient, asset: IssuedAsset, tx: ExchangeTransaction): Result[Unit] =
//    if (blockchain.hasScript(asset)) {
//      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets.id))
//        error.AssetFeatureUnsupported(BlockchainFeatures.SmartAssets, asset).asLeft
//      else
//        blockchain.runScript(asset, tx) match {
//          case RunScriptResult.ScriptError(execError)   => error.AssetScriptReturnedError(asset, execError).asLeft
//          case RunScriptResult.Denied                   => error.AssetScriptDeniedOrder(asset).asLeft
//          case RunScriptResult.Allowed                  => success
//          case RunScriptResult.UnexpectedResult(x)      => error.AssetScriptUnexpectResult(asset, x.toString).asLeft
//          case RunScriptResult.Exception(name, message) => error.AssetScriptException(asset, name, message).asLeft
//        }
//    } else ().asRight

  private def verifySmartToken(blockchain: WavesBlockchainCachingClient, asset: IssuedAsset, tx: ExchangeTransaction)(
      implicit ec: ExecutionContext): FutureResult[Unit] = {

    lazy val verifyAssetScript: FutureResult[Unit] = {

      lazy val verifyScript: FutureResult[Unit] = liftFutureAsync { blockchain.runScript(asset, tx) } map {
        case RunScriptResult.ScriptError(execError)   => error.AssetScriptReturnedError(asset, execError).asLeft
        case RunScriptResult.Denied                   => error.AssetScriptDeniedOrder(asset).asLeft
        case RunScriptResult.Allowed                  => success
        case RunScriptResult.UnexpectedResult(x)      => error.AssetScriptUnexpectResult(asset, x.toString).asLeft
        case RunScriptResult.Exception(name, message) => error.AssetScriptException(asset, name, message).asLeft
      }

      liftFutureAsync { blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets.id) }
        .ifM(verifyScript, liftErrorAsync[Unit] { error.AssetFeatureUnsupported(BlockchainFeatures.SmartAssets, asset) })
    }

    liftFutureAsync { blockchain.hasScript(asset) } ifM (verifyAssetScript, successAsync)
  }

  private def decimals(blockchain: WavesBlockchainCachingClient, asset: Asset)(implicit ec: ExecutionContext): FutureResult[Int] = {
    asset.fold { liftValueAsync(8) } { issuedAsset =>
      EitherT {
        blockchain.assetDescription(issuedAsset).map { _.map(_.decimals).toRight(error.AssetNotFound(issuedAsset)) }
      }
    }
  }

//  private def decimals(blockchain: WavesBlockchainClient, assetId: Asset): Result[Int] =
//    assetId.fold(lift(8)) { aid =>
//      blockchain.assetDescription(aid).map(_.decimals).toRight(error.AssetNotFound(aid))
//    }

  private def validateDecimals(blockchain: WavesBlockchainCachingClient, o: Order)(implicit ec: ExecutionContext): FutureResult[(Int, Int)] = {

    def checkInsignificantDecimals(insignificantDecimals: Int): FutureResult[Unit] = liftAsync {
      cond(o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0, Unit, error.PriceLastDecimalsMustBeZero(insignificantDecimals))
    }

    for {
      pd <- decimals(blockchain, o.assetPair.priceAsset)
      ad <- decimals(blockchain, o.assetPair.amountAsset)
      _  <- checkInsignificantDecimals { (pd - ad).max(0) }
    } yield ad -> pd
  }

  private def validateAmountAndPrice(order: Order, decimalsPair: (Int, Int), orderRestrictions: Map[AssetPair, OrderRestrictionsSettings])(
      implicit ec: ExecutionContext): FutureResult[Order] = {

    if (!(orderRestrictions contains order.assetPair)) liftValueAsync(order)
    else {

      val (amountAssetDecimals, priceAssetDecimals) = decimalsPair
      val restrictions                              = orderRestrictions(order.assetPair)

      def normalizeAmount(amt: Double): Long = Normalization.normalizeAmountAndFee(amt, amountAssetDecimals)
      def normalizePrice(prc: Double): Long  = Normalization.normalizePrice(prc, amountAssetDecimals, priceAssetDecimals)

      liftValueAsync(order)
        .ensure(error.OrderInvalidAmount(order, restrictions)) { o =>
          normalizeAmount(restrictions.minAmount) <= o.amount && o.amount <= normalizeAmount(restrictions.maxAmount) &&
          o.amount % normalizeAmount(restrictions.stepAmount).max(1) == 0
        }
        .ensure(error.OrderInvalidPrice(order, restrictions)) { o =>
          normalizePrice(restrictions.minPrice) <= o.price && o.price <= normalizePrice(restrictions.maxPrice) &&
          o.price % normalizePrice(restrictions.stepPrice).max(1) == 0
        }
    }
  }

  //  private[dex] def checkOrderVersion(version: Byte, blockchain: WavesBlockchainClient): Result[Unit] = version match {
  //    case 1 => success
  //    case 2 =>
  //      if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading.id)) success
  //      else Left(error.OrderVersionUnsupported(version, BlockchainFeatures.SmartAccountTrading))
  //    case 3 =>
  //      if (blockchain.isFeatureActivated(BlockchainFeatures.OrderV3.id)) success
  //      else Left(error.OrderVersionUnsupported(version, BlockchainFeatures.OrderV3))
  //    case _ => Left(error.UnsupportedOrderVersion(version))
  //  }

  private[dex] def checkOrderVersion(version: Byte, blockchain: WavesBlockchainCachingClient)(implicit ec: ExecutionContext): FutureResult[Unit] = {

    def checkFeatureSupport(feature: BlockchainFeature): FutureResult[Unit] = {
      liftFutureAsync { blockchain.isFeatureActivated(feature.id) }
        .ifM(successAsync, liftErrorAsync { error.OrderVersionUnsupported(version, feature) })
    }

    version match {
      case 1 => successAsync
      case 2 => checkFeatureSupport(BlockchainFeatures.SmartAccountTrading)
      case 3 => checkFeatureSupport(BlockchainFeatures.OrderV3)
      case _ => liftErrorAsync { error.UnsupportedOrderVersion(version) }
    }
  }

  def blockchainAware(blockchain: WavesBlockchainClient,
                      asyncBlockchain: WavesBlockchainCachingClient,
                      transactionCreator: (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction],
                      matcherAddress: Address,
                      time: Time,
                      orderFeeSettings: OrderFeeSettings,
                      orderRestrictions: Map[AssetPair, OrderRestrictionsSettings],
                      rateCache: RateCache)(order: Order)(implicit ec: ExecutionContext): FutureResult[Order] = timer.measure {

    lazy val exchangeTx: FutureResult[ExchangeTransaction] = liftAsync {
      val fakeOrder: Order = order.updateType(order.orderType.opposite)
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()) leftMap { x =>
        error.CanNotCreateExchangeTransaction(x.toString)
      }
    }

    def verifyAssetScript(assetId: Asset): FutureResult[Unit] = assetId.fold { successAsync } { assetId =>
      exchangeTx flatMap { verifySmartToken(asyncBlockchain, assetId, _) }
    }

    def verifyMatcherFeeAssetScript(matcherFeeAsset: Asset): FutureResult[Unit] = {
      if (matcherFeeAsset == order.assetPair.amountAsset || matcherFeeAsset == order.assetPair.priceAsset) successAsync
      else verifyAssetScript(matcherFeeAsset)
    }

    // Checks whether order fee is enough to cover matcher's expenses for the Exchange transaction issue
    lazy val validateOrderFeeByTransactionRequirements: FutureResult[Order] = liftAsync {
      orderFeeSettings match {
        case DynamicSettings(baseFee) =>
          val minFee = ExchangeTransactionCreator.minFee(baseFee, blockchain.hasScript(matcherAddress), order.assetPair, blockchain.hasScript)
          val mof =
            multiplyFeeByDouble(
              minFee,
              rateCache.getRate(order.matcherFeeAssetId).getOrElse(throw new RuntimeException(s"Can't find rate for ${order.matcherFeeAssetId}"))
            )
          Either.cond(order.matcherFee >= mof, order, error.FeeNotEnough(mof, order.matcherFee, Waves))
        case _ => lift(order)
      }
    }

    for {
      _            <- checkOrderVersion(order.version, asyncBlockchain)
      _            <- validateOrderFeeByTransactionRequirements
      decimalsPair <- validateDecimals(asyncBlockchain, order)
      _            <- validateAmountAndPrice(order, decimalsPair, orderRestrictions)
      _            <- verifyOrderByAccountScript(asyncBlockchain, order.sender, order)
      _            <- verifyAssetScript(order.assetPair.amountAsset)
      _            <- verifyAssetScript(order.assetPair.priceAsset)
      _            <- verifyMatcherFeeAssetScript(order.matcherFeeAssetId)
    } yield order
  }

  private[dex] def getValidFeeAssetForSettings(order: Order, orderFeeSettings: OrderFeeSettings, rateCache: RateCache): Set[Asset] =
    orderFeeSettings match {
      case _: DynamicSettings               => rateCache.getAllRates.keySet
      case FixedSettings(defaultAssetId, _) => Set(defaultAssetId)
      case PercentSettings(assetType, _) =>
        Set(
          assetType match {
            case AssetType.AMOUNT    => order.assetPair.amountAsset
            case AssetType.PRICE     => order.assetPair.priceAsset
            case AssetType.RECEIVING => order.getReceiveAssetId
            case AssetType.SPENDING  => order.getSpendAssetId
          }
        )
    }

  /**
    * Returns minimal valid fee that should be paid to the matcher when order is placed
    *
    * @param order            placed order
    * @param orderFeeSettings matcher settings for the fee of orders
    * @param matchPrice       price at which order is executed
    * @param rateCache        assets rates (rate = cost of 1 Waves in asset)
    * @param multiplier       coefficient that is used in market aware for specifying deviation bounds
    */
  private[dex] def getMinValidFeeForSettings(order: Order,
                                             orderFeeSettings: OrderFeeSettings,
                                             matchPrice: Long,
                                             rateCache: RateCache,
                                             multiplier: Double = 1): Long = {

    orderFeeSettings match {
      case DynamicSettings(dynamicBaseFee) =>
        multiplyFeeByDouble(
          dynamicBaseFee,
          rateCache.getRate(order.matcherFeeAssetId).getOrElse(throw new RuntimeException(s"Can't find rate for ${order.matcherFeeAssetId}"))
        )
      case FixedSettings(_, fixedMinFee) => fixedMinFee
      case PercentSettings(assetType, minFeeInPercent) =>
        lazy val receiveAmount = order.getReceiveAmount(order.amount, matchPrice).explicitGet()
        lazy val spentAmount   = order.getSpendAmount(order.amount, matchPrice).explicitGet()

        val amount = assetType match {
          case AssetType.AMOUNT    => order.amount
          case AssetType.PRICE     => if (order.orderType == OrderType.BUY) spentAmount else receiveAmount
          case AssetType.RECEIVING => receiveAmount
          case AssetType.SPENDING  => spentAmount
        }

        multiplyAmountByDouble(amount, multiplier * minFeeInPercent / 100)
    }
  }

  private def validateOrderFee(order: Order, orderFeeSettings: OrderFeeSettings, rateCache: RateCache): Result[Order] = {
    if (order.version < 3) lift(order)
    else {
      lazy val requiredFeeAssetIds = getValidFeeAssetForSettings(order, orderFeeSettings, rateCache)
      lazy val requiredFee         = getMinValidFeeForSettings(order, orderFeeSettings, order.price, rateCache)

      lift(order)
        .ensure(error.UnexpectedFeeAsset(requiredFeeAssetIds, order.matcherFeeAssetId))(o => requiredFeeAssetIds contains o.matcherFeeAssetId)
        .ensure(error.FeeNotEnough(requiredFee, order.matcherFee, order.matcherFeeAssetId))(_.matcherFee >= requiredFee)
    }
  }

  def matcherSettingsAware(matcherPublicKey: PublicKey,
                           blacklistedAddresses: Set[Address],
                           blacklistedAssets: Set[IssuedAsset],
                           matcherSettings: MatcherSettings,
                           rateCache: RateCache)(order: Order): Result[Order] = {

    def validateBlacklistedAsset(assetId: Asset, e: IssuedAsset => MatcherError): Result[Unit] =
      assetId.fold(success)(x => cond(!blacklistedAssets(x), (), e(x)))

    for {
      _ <- lift(order)
        .ensure(error.UnexpectedMatcherPublicKey(matcherPublicKey, order.matcherPublicKey))(_.matcherPublicKey == matcherPublicKey)
        .ensure(error.AddressIsBlacklisted(order.sender))(o => !blacklistedAddresses.contains(o.sender.toAddress))
        .ensure(error.OrderVersionDenied(order.version, matcherSettings.allowedOrderVersions)) { o =>
          matcherSettings.allowedOrderVersions(o.version)
        }
      _ <- validateBlacklistedAsset(order.matcherFeeAssetId, error.FeeAssetBlacklisted)
      _ <- validateOrderFee(order, matcherSettings.orderFee, rateCache)
    } yield order
  }

  /**
    * Checks if price is in deviation bounds
    *
    *   For BUY orders:  (1 - p) * best bid <= price <= (1 + l) * best ask
    *   For SELL orders: (1 - l) * best bid <= price <= (1 + p) * best ask
    *
    * where:
    *
    *   p = max price deviation profit / 100
    *   l = max price deviation loss / 100
    *   best bid = highest price of buy
    *   best ask = lowest price of sell
    */
  private def validatePriceDeviation(order: Order, deviationSettings: DeviationsSettings, marketStatus: Option[MarketStatus]): Result[Order] = {

    def isPriceInDeviationBounds(subtractedPercent: Double, addedPercent: Double): Boolean = marketStatus forall { ms =>
      lazy val isPriceHigherThanMinDeviation = ms.bestBid forall { bestBid =>
        order.price >= multiplyPriceByDouble(bestBid.price, 1 - (subtractedPercent / 100))
      }

      lazy val isPriceLessThanMaxDeviation = ms.bestAsk forall { bestAsk =>
        order.price <= multiplyPriceByDouble(bestAsk.price, 1 + (addedPercent / 100))
      }

      isPriceHigherThanMinDeviation && isPriceLessThanMaxDeviation
    }

    lift(order).ensure { error.DeviantOrderPrice(order, deviationSettings) } { _ =>
      if (order.orderType == OrderType.BUY) isPriceInDeviationBounds(deviationSettings.maxPriceProfit, deviationSettings.maxPriceLoss)
      else isPriceInDeviationBounds(deviationSettings.maxPriceLoss, deviationSettings.maxPriceProfit)
    }
  }

  /**
    * Checks if fee is in deviation bounds, i.e. orders's fee is higher than the specified percentage of fee,
    * which the client would pay for the matching with the best counter order. Only applicable to the `percent` order fee mode.
    *
    *   For BUY orders:  fee >= fs * (1 - fd) * best ask * amount
    *   For SELL orders: fee >= fs * (1 - fd) * best bid * amount
    *
    * where:
    *
    *   fs = fee in percents from order-fee settings (order-fee.percent.min-fee) / 100
    *   fd = max fee deviation / 100
    *   best bid = highest price of buy
    *   best ask = lowest price of sell
    */
  private def validateFeeDeviation(order: Order,
                                   deviationSettings: DeviationsSettings,
                                   orderFeeSettings: OrderFeeSettings,
                                   marketStatus: Option[MarketStatus],
                                   rateCache: RateCache): Result[Order] = {

    def isFeeInDeviationBoundsForMatchedPrice(matchedPrice: Long): Boolean = orderFeeSettings match {
      case percentSettings: PercentSettings =>
        order.matcherFee >= getMinValidFeeForSettings(order, percentSettings, matchedPrice, rateCache, 1 - (deviationSettings.maxFeeDeviation / 100))
      case _ => true
    }

    val isFeeInDeviationBounds = marketStatus forall { ms =>
      (order.orderType, ms.bestAsk.isDefined, ms.bestBid.isDefined) match {
        case (OrderType.BUY, true, _)  => isFeeInDeviationBoundsForMatchedPrice(ms.bestAsk.get.price) // validate fee for the best (lowest) sell price
        case (OrderType.SELL, _, true) => isFeeInDeviationBoundsForMatchedPrice(ms.bestBid.get.price) // validate fee for the best (highest) buy price
        case _                         => true
      }
    }

    Either.cond(isFeeInDeviationBounds, order, error.DeviantOrderMatcherFee(order, deviationSettings))
  }

  def marketAware(orderFeeSettings: OrderFeeSettings,
                  deviationSettings: DeviationsSettings,
                  marketStatus: Option[MarketStatus],
                  rateCache: RateCache)(order: Order): Result[Order] = {
    if (deviationSettings.enabled) {
      for {
        _ <- validatePriceDeviation(order, deviationSettings, marketStatus)
        _ <- validateFeeDeviation(order, deviationSettings, orderFeeSettings, marketStatus, rateCache)
      } yield order
    } else lift(order)
  }

  def timeAware(time: Time)(order: Order): Result[Order] = {
    for {
      _ <- cond(order.expiration > time.correctedTime + MinExpiration,
                (),
                error.WrongExpiration(time.correctedTime(), MinExpiration, order.expiration))
      _ <- order.isValid(time.correctedTime()).toEither.leftMap(error.OrderCommonValidationFailed)
    } yield order
  }

  private def validateBalance(acceptedOrder: AcceptedOrder,
                              tradableBalance: Asset => Long,
                              orderBookCache: AssetPair => OrderBook.AggregatedSnapshot): Result[AcceptedOrder] = {

    /**
      * According to the current market state calculates cost for buy market orders or amount for sell market orders
      * that should be covered by tradable balance of the order's owner.
      * Returns InvalidMarketOrderPrice error in case of too low price of buy orders or too high price of sell orders
      */
    def getMarketOrderValue: Result[Long] = {

      // Adds value of level to the current value of the market order
      def accumulateLevel(level: LevelAgg, moValue: Result[Long], remainToExecute: Long): (Result[Long], Long) = {
        val levelValue: Long => Long = amount => if (acceptedOrder.isBuyOrder) MatcherModel.getCost(amount, level.price) else amount
        if (remainToExecute >= level.amount) moValue.map { _ + levelValue(level.amount) } -> (remainToExecute - level.amount)
        else moValue.map { _ + levelValue(remainToExecute) }                              -> 0L
      }

      @tailrec
      def go(levels: Seq[LevelAgg], currentValue: Result[Long], remainToExecute: Long): (Result[Long], Long) = {
        (levels.headOption, currentValue) match {
          case (_, value) if value.isLeft   => value                -> remainToExecute
          case (None, fullMarketOrderValue) => fullMarketOrderValue -> remainToExecute
          case (Some(level), value) =>
            val isLevelPriceMatchable = if (acceptedOrder.isBuyOrder) acceptedOrder.price >= level.price else acceptedOrder.price <= level.price
            (isLevelPriceMatchable, remainToExecute > 0) match {
              case (true, true)  => val (newVal, newRTE) = accumulateLevel(level, value, remainToExecute); go(levels.tail, newVal, newRTE)
              case (false, true) => error.InvalidMarketOrderPrice(acceptedOrder.order).asLeft[Long] -> remainToExecute
              case _             => value -> remainToExecute
            }
        }
      }

      go(orderBookCache(acceptedOrder.order.assetPair).getCounterSideFor(acceptedOrder), 0L.asRight[MatcherError], acceptedOrder.amount)._1
    }

    def getRequiredBalanceForMarketOrder(marketOrder: MarketOrder, marketOrderValue: Long): Map[Asset, Long] = {
      Map(marketOrder.spentAsset -> marketOrderValue) |+| Map(marketOrder.feeAsset -> marketOrder.requiredFee)
    }

    def validateTradableBalance(requiredForOrder: Map[Asset, Long]): Result[AcceptedOrder] = {
      val availableBalances = acceptedOrder.availableBalanceBySpendableAssets(tradableBalance)
      val negativeBalances  = availableBalances |+| requiredForOrder.mapValues { -_ } filter { case (_, balance) => balance < 0 }
      cond(negativeBalances.isEmpty, acceptedOrder, error.BalanceNotEnough(requiredForOrder, availableBalances))
    }

    acceptedOrder match {
      case mo: MarketOrder => getMarketOrderValue >>= (volume => validateTradableBalance { getRequiredBalanceForMarketOrder(mo, volume) })
      case _               => validateTradableBalance(acceptedOrder.requiredBalance)
    }
  }

  def accountStateAware(sender: Address,
                        tradableBalance: Asset => Long,
                        activeOrderCount: => Int,
                        orderExists: ByteStr => Boolean,
                        orderBookCache: AssetPair => OrderBook.AggregatedSnapshot)(acceptedOrder: AcceptedOrder): Result[AcceptedOrder] =
    for {
      _ <- lift(acceptedOrder)
        .ensure(error.UnexpectedSender(acceptedOrder.order.sender.toAddress, sender))(_.order.sender.toAddress == sender)
        .ensure(error.ActiveOrdersLimitReached(MaxActiveOrders))(_ => activeOrderCount < MaxActiveOrders)
        .ensure(error.OrderDuplicate(acceptedOrder.order.id()))(ao => !orderExists(ao.order.id()))
      _ <- validateBalance(acceptedOrder, tradableBalance, orderBookCache)
    } yield acceptedOrder

  private def lift[T](x: T): Result[T] = x.asRight[MatcherError]

  def liftAsync[T](x: Result[T]): FutureResult[T] = EitherT { Future.successful(x) }

  private def liftValueAsync[T](x: T): FutureResult[T]                                         = EitherT { Future.successful(x.asRight[MatcherError]) }
  private def liftErrorAsync[T](x: MatcherError): FutureResult[T]                              = EitherT { Future.successful(x.asLeft[T]) }
  private def liftFutureAsync[T](x: Future[T])(implicit ex: ExecutionContext): FutureResult[T] = EitherT.right[MatcherError](x)

  private def success: Result[Unit]            = lift(Unit)
  private def successAsync: FutureResult[Unit] = liftValueAsync(Unit)
}
