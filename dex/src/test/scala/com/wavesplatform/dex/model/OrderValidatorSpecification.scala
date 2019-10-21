package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderBook.AggregatedSnapshot
import com.wavesplatform.dex.model.OrderValidator.Result
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, FixedSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, DeviationsSettings, OrderRestrictionsSettings}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.{NoShrink, TestTime, WithDB}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(btc -> 10 * Constants.UnitsInWave))

  private implicit val errorContext: ErrorFormatterContext = _ => defaultAssetDecimals

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in asa() { v =>
      v shouldBe 'right
    }

    "reject new order" when {
      "this order had already been accepted" in asa(orderStatus = _ => true) { v =>
        v should produce("OrderDuplicate")
      }

      "sender's address is blacklisted" in {
        val blacklistedAccount = KeyPair("3irbW78fffj5XDzAMjaEeo3kn8V".getBytes(Charsets.UTF_8))
        val o                  = newBuyOrder(blacklistedAccount)

        val v = msa(Set(blacklistedAccount.toAddress), o)
        v(o) should produce("AddressIsBlacklisted")
      }

      "v1 order from a scripted account" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Allowed)
          assignNoScript(bc, btc)
          assignAssetDescription(bc, btc -> mkAssetDescription(8))
          activate(bc, _ => false)

          ov(newBuyOrder(scripted)) should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Allowed)
          assignNoScript(bc, btc)
          assignAssetDescription(bc, btc -> mkAssetDescription(8))
          activate(bc, _ => false)

          ov(newBuyOrder(scripted)) should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Denied)
          assignNoScript(bc, btc)
          assignAssetDescription(bc, btc -> mkAssetDescription(8))
          activate(bc, _ == BlockchainFeatures.SmartAccountTrading.id)

          ov(newBuyOrder(scripted, version = 2)) should produce("AccountScriptDeniedOrder")
        }
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp() + offset).updateSender(pk), pk)

        OrderValidator.timeAware(tt)(signed) should produce("WrongExpiration")
      }

      "amount is invalid" in {
        val pk = KeyPair(randomBytes())
        val unsigned = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(amount = 0L)
          case x: OrderV2 => x.copy(amount = 0L)
        }
        val signed = Order.sign(unsigned, pk)
        OrderValidator.timeAware(ntpTime)(signed).left.map(_.toJson(errorContext)) should produce("amount should be > 0")
      }

      "order signature is invalid" in portfolioTest(defaultPortfolio) { (ov, bc) =>
        val pk = KeyPair(randomBytes())
        assignNoScript(bc, pk.toAddress)
        assignNoScript(bc, btc)
        assignAssetDescription(bc, btc -> mkAssetDescription(8))
        val order = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
          case x: OrderV2 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
        }
        ov(order) should produce("InvalidSignature")
      }

      "order exists" in {
        val pk = KeyPair(randomBytes())
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true, _ => OrderBook.AggregatedSnapshot())(_)
        ov(LimitOrder(newBuyOrder(pk, 1000))) should produce("OrderDuplicate")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetIdGen(1), accountGen, Gen.choose(1, 7)) {
        case (amountAsset, sender, amountDecimals) =>
          portfolioTest(Portfolio(11 * Constants.UnitsInWave, LeaseBalance.empty, Map.empty)) { (ov, bc) =>
            assignNoScript(bc, sender.toAddress)
            assignAssetDescription(bc, amountAsset -> mkAssetDescription(amountDecimals))

            val price = BigDecimal(10).pow(-amountDecimals - 1)
            ov(
              buy(
                AssetPair(amountAsset, Waves),
                10 * Constants.UnitsInWave,
                price,
                matcherFee = Some((0.003 * Constants.UnitsInWave).toLong)
              )) should produce("PriceLastDecimalsMustBeZero")
          }
      }

      "matcherFeeAssetId is blacklisted" in {
        val preconditions = for {
          matcherFeeAsset <- arbitraryAssetIdGen map (asset => IssuedAsset(asset.compatId.get))
          (_, order)      <- orderV3WithPredefinedFeeAssetGenerator(Some(matcherFeeAsset))
        } yield order -> matcherFeeAsset

        forAll(preconditions) {
          case (order, matcherFeeAssetId) =>
            validateByMatcherSettings(matcherSettings.orderFee, Set(matcherFeeAssetId))(order) should produce("FeeAssetBlacklisted")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (percent mode and arbitrary asset)" in {
        val preconditions = for {
          (_, order)      <- orderV3WithPredefinedFeeAssetGenerator()
          percentSettings <- percentSettingsGenerator
        } yield order -> percentSettings

        // in percent mode it's not allowed to pay fee in arbitrary asset (only in one of the assets of the pair)

        forAll(preconditions) {
          case (order, percentFeeSettings) => validateByMatcherSettings(percentFeeSettings)(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (fixed mode and incorrect asset)" in {
        val preconditions =
          for {
            order            <- orderV3Generator
            fixedFeeAsset    <- arbitraryAssetIdGen
            fixedFeeSettings <- fixedSettingsGenerator(fixedFeeAsset)
          } yield (order, fixedFeeSettings)

        forAll(preconditions) {
          case (order, fixedFeeSettings) => validateByMatcherSettings(fixedFeeSettings)(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (dynamic mode and incorrect asset)" in {
        forAll(orderV3WithPredefinedFeeAssetGenerator()) {
          case (_, order) => validateByMatcherSettings(DynamicSettings(order.matcherFee))(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFee is not enough (percent mode)" in {
        def validateByPercentSettings(assetType: AssetType): Order => Result[Order] = validateByMatcherSettings { PercentSettings(assetType, 0.3) }

        withClue("AMOUNT/RECEIVING asset type, min fee = 0.3%, fee should be >= 1.5.waves\n") {
          val order = createOrder(pairWavesBtc, OrderType.BUY, 500.waves, price = 0.00011162, matcherFee = 1.5.waves, matcherFeeAsset = Waves)
          Seq(AssetType.AMOUNT, AssetType.RECEIVING).foreach { assetType =>
            validateByPercentSettings(assetType) { order } shouldBe 'right
            validateByPercentSettings(assetType) { order.updateFee(1.49999999.waves) } should produce("FeeNotEnough")
          }
        }

        withClue("PRICE/SPENDING asset type, min fee = 0.3%, fee should be >= 0.00016743.btc\n") {
          val order = createOrder(pairWavesBtc, OrderType.BUY, 500.waves, price = 0.00011162, matcherFee = 0.00016743.btc, matcherFeeAsset = btc)
          Seq(AssetType.PRICE, AssetType.SPENDING).foreach { assetType =>
            validateByPercentSettings(assetType) { order } shouldBe 'right
            validateByPercentSettings(assetType) { order.updateFee(0.00016742.btc) } should produce("FeeNotEnough")
          }
        }
      }

      "matcherFee is not enough (fixed mode)" in {
        val validateByFixedSettings: Order => Result[Order] = validateByMatcherSettings { FixedSettings(usd, 0.03.usd) }
        withClue("Fee should be >= 0.03.usd\n") {
          val order = createOrder(pairWavesBtc, OrderType.BUY, 100.waves, price = 0.00011162, matcherFee = 0.03.usd, matcherFeeAsset = usd)
          validateByFixedSettings { order } shouldBe 'right
          validateByFixedSettings { order.updateFee(0.02.usd) } should produce("FeeNotEnough")
        }
      }

      "matcherFee is not enough (dynamic mode)" in {
        val validateByDynamicSettings: Order => Result[Order] = validateByMatcherSettings(DynamicSettings(0.003.waves), rateCache = rateCache)

        /**
          * asset rate = price of 1 Waves in that asset;
          * fee should be >= base fee * 10 pow (fee asset decimals - 8) * rate, ceiling round mode
          */
        withClue("Fee in USD (2 decimals) should be >= 0.02.usd\n") {
          val order = createOrder(pairWavesUsd, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.02.usd, matcherFeeAsset = usd)
          validateByDynamicSettings { order } shouldBe 'right
          validateByDynamicSettings { order.updateFee(0.01.usd) } should produce("FeeNotEnough")
        }

        withClue("Fee in BTC (8 decimals) should be >= 0.00000034.btc\n") {
          val order = createOrder(pairWavesBtc, OrderType.BUY, 100.waves, price = 0.00011162, matcherFee = 0.00000034.btc, matcherFeeAsset = btc)
          validateByDynamicSettings { order } shouldBe 'right
          validateByDynamicSettings { order.updateFee(0.00000033.btc) } should produce("FeeNotEnough")
        }
      }

      "matcherFee is not enough (blockchain aware)" in {

        def validateFeeByBlockchain(priceAssetScript: Option[RunScriptResult] = None,
                                    matcherScript: Option[RunScriptResult] = None): Order => Result[Order] = {
          validateByBlockchain { DynamicSettings(0.003.waves) }(priceAssetScript = priceAssetScript,
                                                                matcherAccountScript = matcherScript,
                                                                rateCache = rateCache)
        }

        def updateOrder(order: Order, f: Order => Order): Order = Order.sign(f(order), senderKeyPair)

        withClue(s"Fee in USD (2 decimals, rate = 3.7) ") {

          val order  = createOrder(pairWavesUsd, OrderType.BUY, 1.waves, price = 3.7, matcherFee = 0.02.usd, matcherFeeAsset = usd)
          val script = Some { RunScriptResult.Allowed }

          withClue(s"without any scripts should be >= 0.02.usd\n") {
            validateFeeByBlockchain() { order } shouldBe 'right
          }

          /**
            * min fee = (base fee +
            *            extra fee for amount asset script (here 0 since amount asset = Waves) +
            *            extra fee for price asset script +
            *            extra fee for matcher account script) * rate * 10 pow (fee asset decimals - 8) rounding mode ceiling
            *
            * where
            *   base fee                  = 0.003.waves = 300000,
            *   extra fee for any scripts = 0.004.waves = 400000
            */
          withClue(s"with price asset script should be >= 2.59 rounding mode ceiling = 0.03.usd\n") {
            validateFeeByBlockchain(script) { order } should produce("FeeNotEnough")
            validateFeeByBlockchain(script) { updateOrder(order, _.updateFee(0.03.usd)) } shouldBe 'right
          }

          withClue(s"with price asset and matcher account script should be >= 4.07 round mode ceiling = 0.05.usd\n") {
            validateFeeByBlockchain(script, script) { updateOrder(order, _.updateFee(0.04.usd)) } should produce("FeeNotEnough")
            validateFeeByBlockchain(script, script) { updateOrder(order, _.updateFee(0.05.usd)) } shouldBe 'right
          }
        }
      }

      "it's version doesn't meet matcher's requirements" in {

        def orderOfVersion(version: Byte): Order = {
          createOrder(pairWavesUsd, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.003.waves, version = version, matcherFeeAsset = Waves)
        }

        Seq[Byte](1, 2, 3) foreach { version =>
          validateByMatcherSettings { FixedSettings(usd, 0.01.usd) } { orderOfVersion(version) } should produce("UnexpectedFeeAsset")
          validateByMatcherSettings { FixedSettings(Waves, 0.003.waves) } { orderOfVersion(version) } shouldBe 'right

          validateByMatcherSettings { PercentSettings(AssetType.PRICE, 0.003) } { orderOfVersion(version) } should produce("UnexpectedFeeAsset")
          validateByMatcherSettings { PercentSettings(AssetType.AMOUNT, 0.003) } { orderOfVersion(version) } shouldBe 'right
        }
      }

      "matcherFee is insufficient in case of scripted account or asset" in forAll(orderWithoutWavesInPairAndWithFeeSettingsGenerator) {
        case (order, _, orderFeeSettings) =>
          val trueScript = RunScriptResult.Allowed

          def setAssetsAndMatcherAccountScriptsAndValidate(amountAssetScript: Option[RunScriptResult],
                                                           priceAssetScript: Option[RunScriptResult],
                                                           matcherAccountScript: Option[RunScriptResult]): Result[Order] =
            validateByBlockchain(orderFeeSettings)(amountAssetScript, priceAssetScript, None, matcherAccountScript)(order)

          orderFeeSettings match {
            case _: DynamicSettings =>
              setAssetsAndMatcherAccountScriptsAndValidate(Some(trueScript), None, None) should produce("FeeNotEnough")
              setAssetsAndMatcherAccountScriptsAndValidate(None, Some(trueScript), None) should produce("FeeNotEnough")
              setAssetsAndMatcherAccountScriptsAndValidate(None, None, Some(trueScript)) should produce("FeeNotEnough")

              setAssetsAndMatcherAccountScriptsAndValidate(None, None, None) shouldBe 'right

            case _ =>
              setAssetsAndMatcherAccountScriptsAndValidate(Some(trueScript), None, None) shouldBe 'right
              setAssetsAndMatcherAccountScriptsAndValidate(None, Some(trueScript), None) shouldBe 'right
              setAssetsAndMatcherAccountScriptsAndValidate(None, None, Some(trueScript)) shouldBe 'right

              setAssetsAndMatcherAccountScriptsAndValidate(None, None, None) shouldBe 'right
          }
      }

      "order's price is out of deviation bounds (market aware)" in {
        val deviationSettings = DeviationsSettings(enabled = true, maxPriceProfit = 50, maxPriceLoss = 70, maxFeeDeviation = 50)
        val orderFeeSettings  = DynamicSettings(0.003.waves)

        val buyOrder  = createOrder(pairWavesBtc, OrderType.BUY, amount = 250.waves, price = 0.00011081)
        val sellOrder = createOrder(pairWavesBtc, OrderType.SELL, amount = 250.waves, price = 0.00011081)

        val bestAsk = LevelAgg(amount = 800.waves, price = 0.00011082.btc)
        val bestBid = LevelAgg(amount = 600.waves, price = 0.00011080.btc)

        /**
          * BUY orders:  (1 - p) * best bid <= price <= (1 + l) * best ask
          * SELL orders: (1 - l) * best bid <= price <= (1 + p) * best ask
          *
          * where:
          *
          *   p = max price deviation profit / 100
          *   l = max price deviation loss / 100
          *   best bid = highest price of buy
          *   best ask = lowest price of sell
          */
        val lowSellOrderPrices = Array(0, 0.00000001, 0.00000011, 0.000015, 0.00003322, 0.00002999, 0.00003299, 0.00003319, 0.00003323)
        val midSellOrderPrices = Array(0.00003324, 0.00003325, 0.00003999, 0.00006648, 0.00016622, 0.00016623)
        val highSellOrderPrices = Array(0.00016624, 0.0001671, 0.00016633, 0.000167, 0.00017, 0.00033248, 0.0009, 0.00123123, 0.12312311, 1.12312312,
          123.12312123, 100000.1, 100000.123123, 12345678.12347894)

        val lowBuyOrderPrices = Array(0, 0.00000001, 0.00000033, 0.00000450, 0.0000045, 0.00002770, 0.0000277, 0.00005539)
        val midBuyOrderPrices =
          Array(0.00005540, 0.00005541, 0.00005580, 0.00005641, 0.00006, 0.0001, 0.00017999, 0.00018799, 0.00018829, 0.00018838, 0.00018839)
        val highBuyOrderPrices = Array(0.00018840, 0.00018841, 0.00037678, 0.00123456, 0.01951753, 0.98745612, 1, 1.12345678, 5000.12341234, 100000.1,
          100000.1234, 100000.1234789, 12345678.12347894)

        val priceValidationWithNoBounds =
          OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, None, None)), rateCache) _

        withClue("order price can be any if bids & asks don't exist") {
          for (order <- Array(buyOrder, sellOrder)) {
            priceValidationWithNoBounds { order } shouldBe 'right
            (lowSellOrderPrices ++ midSellOrderPrices ++ highSellOrderPrices).foreach(price =>
              priceValidationWithNoBounds { order.updatePrice(price.btc) } shouldBe 'right)
          }
        }

        val priceValidationWithLowerBound =
          OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, Some(bestBid), None)), rateCache) _
        withClue("order price has only lower bound if there are no asks") {
          priceValidationWithNoBounds { buyOrder } shouldBe 'right
          lowBuyOrderPrices.foreach(price => priceValidationWithLowerBound { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
          (midBuyOrderPrices ++ highBuyOrderPrices).foreach(price =>
            priceValidationWithLowerBound { buyOrder.updatePrice(price.btc) } shouldBe 'right)
          priceValidationWithNoBounds { sellOrder } shouldBe 'right
          (lowSellOrderPrices).foreach(price =>
            priceValidationWithLowerBound { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
          (midSellOrderPrices ++ highSellOrderPrices).foreach(price =>
            priceValidationWithLowerBound { sellOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        val priceValidationWithUpperBound =
          OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, None, Some(bestAsk))), rateCache) _
        withClue("order price has only upper bound if there are no bids") {
          priceValidationWithNoBounds { buyOrder } shouldBe 'right
          highBuyOrderPrices.foreach(price => priceValidationWithUpperBound { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
          (lowBuyOrderPrices ++ midBuyOrderPrices).foreach(price => priceValidationWithUpperBound { buyOrder.updatePrice(price.btc) } shouldBe 'right)

          priceValidationWithNoBounds { sellOrder } shouldBe 'right
          highSellOrderPrices.foreach(price => priceValidationWithUpperBound { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
          (lowSellOrderPrices ++ midSellOrderPrices).foreach(price =>
            priceValidationWithUpperBound { sellOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        val nonEmptyMarketStatus = MarketStatus(None, Some(bestBid), Some(bestAsk))
        val priceValidation      = OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(nonEmptyMarketStatus), rateCache) _

        priceValidation { buyOrder } shouldBe 'right

        withClue("buy order price should be >= 0.5 * best bid = 0.5 * 0.00011080.btc = 0.00005540.btc\n") {
          lowBuyOrderPrices.foreach(price => priceValidation { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }

        withClue("0.5 * best bid <= buy order price <= 1.7 * best ask (0.00005540.btc <= price <= 0.00018839.btc)\n") {
          midBuyOrderPrices.foreach(price => priceValidation { buyOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        withClue("buy order price should be <= 1.7 * best ask = 1.7 * 0.00011082.btc = 0.00018839.btc\n") {
          highBuyOrderPrices.foreach(price => priceValidation { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }

        priceValidation { sellOrder } shouldBe 'right

        withClue("sell order price should be >= 0.3 * best bid = 0.3 * 0.00011080.btc = 0.00003324.btc\n") {
          lowSellOrderPrices.foreach(price => priceValidation { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }

        withClue("0.3 * best bid <= sell order price <= 1.5 * best ask (0.00003324.btc <= price <= 0.00016623.btc)\n") {
          midSellOrderPrices.foreach(price => priceValidation { sellOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        withClue("sell order price should be <= 1.5 * best ask = 1.5 * 0.00011082.btc = 0.00016623.btc\n") {
          highSellOrderPrices.foreach(price => priceValidation { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }
      }

      "order's fee is out of deviation bounds (market aware)" in {

        /**
          * BUY orders:  fee >= fs * (1 - fd) * best ask * amount
          * SELL orders: fee >= fs * (1 - fd) * best bid * amount
          *
          * where:
          *
          *   fs = fee in percents from order-fee settings (order-fee.percent.min-fee) / 100
          *   fd = max fee deviation / 100
          *   best bid = highest price of buy
          *   best ask = lowest price of sell
          */
        val bestAsk = LevelAgg(amount = 800.waves, price = 0.00011082.btc)
        val bestBid = LevelAgg(amount = 600.waves, price = 0.00011080.btc)

        val percentSettings      = PercentSettings(AssetType.PRICE, 1) // matcher fee = 1% of the deal
        val deviationSettings    = DeviationsSettings(enabled = true, 100, 100, maxFeeDeviation = 10) // fee deviation = 10%
        val nonEmptyMarketStatus = MarketStatus(None, Some(bestBid), Some(bestAsk))
        val feeValidation        = OrderValidator.marketAware(percentSettings, deviationSettings, Some(nonEmptyMarketStatus), rateCache) _
        val feeValidationWithoutAsks =
          OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, Some(bestBid), None)), rateCache) _
        val feeValidationWithoutBids =
          OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, None, Some(bestAsk))), rateCache) _
        val feeValidationWithoutBounds =
          OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, None, None)), rateCache) _

        // matherFee = 1% of (amount * price) = 0.000277025 => 0.00027702
        val buyOrder =
          createOrder(pairWavesBtc, OrderType.BUY, amount = 250.waves, price = 0.00011081, matcherFee = 0.00027702.btc, matcherFeeAsset = btc)
        val sellOrder =
          createOrder(pairWavesBtc, OrderType.SELL, amount = 250.waves, price = 0.00011081, matcherFee = 0.00027702.btc, matcherFeeAsset = btc)
        val lowBuyOrdersFees   = Array(0, 0.00000001, 0.00001, 0.0001, 0.00012467, 0.00019999, 0.00023999, 0.00024899, 0.00024929, 0.00024934)
        val validBuyOrdersFees = Array(0.00024935, 0.00024936, 0.0002494, 0.00025001, 0.0003, 0.00123123, 1.1231231, 123123.1, 123123.12312312)

        feeValidation { buyOrder } shouldBe 'right

        withClue("buy order fee can be any if there is no asks") {
          (lowBuyOrdersFees ++ validBuyOrdersFees).foreach(fee => {
            val updatedOrder = buyOrder.updateFee(fee.btc)
            feeValidationWithoutAsks { updatedOrder } shouldBe 'right
            feeValidationWithoutBounds { updatedOrder } shouldBe 'right
          })
        }

        withClue("buy order fee should be >= 0.01 * 0.9 * best ask * amount = 0.01 * 0.9 * 0.00011082.btc * 250 = 0.00024935.btc\n") {
          lowBuyOrdersFees.foreach(fee => feeValidation { buyOrder.updateFee(fee.btc) } should produce("DeviantOrderMatcherFee"))
        }

        withClue("buy order fee >= 0.01 * 0.9 * best ask * amount = 0.01 * 0.9 * 0.00011082.btc * 250 = 0.00024935.btc\n") {
          validBuyOrdersFees.foreach(fee => feeValidation { buyOrder.updateFee(fee.btc) } shouldBe 'right)
        }

        val lowSellOrdersFees   = Array(0, 0.00000001, 0.00001, 0.0001, 0.00012467, 0.00019999, 0.00023999, 0.00024899, 0.00024929)
        val validSellOrdersFees = Array(0.00024930, 0.00024931, 0.00024940, 0.00025, 0.0003, 0.00123123, 1.1231231, 123123.1, 123123.12312312)

        feeValidation { sellOrder } shouldBe 'right

        withClue("sell order fee can be any if there is no bids") {
          (lowSellOrdersFees ++ validSellOrdersFees).foreach(fee => {
            val updatedOrder = sellOrder.updateFee(fee.btc)
            feeValidationWithoutBids { updatedOrder } shouldBe 'right
            feeValidationWithoutBounds { updatedOrder } shouldBe 'right
          })
        }

        withClue("sell order fee should be >= 0.01 * 0.9 * best bid * amount = 0.01 * 0.9 * 0.00011080.btc * 250 = 0.00024930.btc\n") {
          lowSellOrdersFees.foreach(fee => feeValidation { sellOrder.updateFee(fee.btc) } should produce("DeviantOrderMatcherFee"))
        }

        withClue("sell order fee >= 0.01 * 0.9 * best bid * amount = 0.01 * 0.9 * 0.00011080.btc * 250 = 0.00024930.btc\n") {
          validSellOrdersFees.foreach(fee => feeValidation { sellOrder.updateFee(fee.btc) } shouldBe 'right)
        }
      }

      "it's version is not allowed by matcher" in {

        def orderOfVersion(version: Byte): Order = {
          createOrder(pairWavesUsd, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.003.waves, version = version, matcherFeeAsset = Waves)
        }

        def validate(allowedOrderVersions: Set[Byte]): Order => Result[Order] = {
          validateByMatcherSettings(DynamicSettings(0.003.waves), allowedOrderVersions = allowedOrderVersions)
        }

        validate { Set(1) } { orderOfVersion(2) } should produce("OrderVersionDenied")
        validate { Set(1, 2) } { orderOfVersion(3) } should produce("OrderVersionDenied")

        Seq[Byte](1, 2, 3).foreach { version =>
          validate(Set(1, 2, 3)) { orderOfVersion(version) } shouldBe 'right
        }
      }

      "it's price is less than the tick size (for buy orders)" in {

        def normalizePrice(denormalizedPrice: Double): Long = {
          Normalization.normalizePrice(value = denormalizedPrice,
                                       amountAssetDecimals = getDefaultAssetDecimals(Waves),
                                       priceAssetDecimals = getDefaultAssetDecimals(usd))
        }

        val validateByTickSize: Order => Result[Order] = OrderValidator.tickSizeAware { normalizePrice(3) }

        withClue(s"Tick size = 3, order price should be >= 3\n") {

          val buyOrder             = createOrder(pairWavesUsd, OrderType.BUY, amount = 1.waves, price = 3.2)
          val badPriceForBuyOrders = normalizePrice(2.99)

          validateByTickSize { buyOrder } shouldBe 'right
          validateByTickSize { buyOrder.updatePrice(badPriceForBuyOrders) } should produce("OrderInvalidPriceLevel")
          validateByTickSize { buyOrder.updateType(OrderType.SELL).updatePrice(badPriceForBuyOrders) } shouldBe 'right
        }
      }

      "amount or price does not meet matcher's settings requirements" in {
        val orderRestrictions = Map { pairWavesUsd -> OrderRestrictionsSettings(0.5, 0.5, 100, 0.5, 0.5, 100) }

        def orderWith(amount: Long, price: Double): Order = createOrder(pairWavesUsd, OrderType.BUY, amount, price)

        def validateByAmountAndPrice(orderRestrictions: Map[AssetPair, OrderRestrictionsSettings] = orderRestrictions): Order => Result[Order] = {
          validateByBlockchain(DynamicSettings(0.003.waves), orderRestrictions)()
        }

        validateByAmountAndPrice() { orderWith(amount = 50.waves, price = 3) } shouldBe 'right

        withClue(s"Amount restrictions for the Waves/USD pair: step amount = 0.5.waves, min amount = 0.5.waves, max amount = 100.waves\n") {
          Seq(
            orderWith(amount = 0.49999999.waves, price = 3), // too low amount
            orderWith(amount = 0.50000001.waves, price = 3), // isn't a multiple of the step amount
            orderWith(amount = 100.00000001.waves, price = 3) // too high amount
          ) foreach { order =>
            validateByAmountAndPrice(Map.empty) { order } shouldBe 'right
            validateByAmountAndPrice() { order } should produce("OrderInvalidAmount")
          }
        }

        withClue(s"Price restrictions for the Waves/USD pair: step price = 0.5, min price = 0.5, max price = 100\n") {
          Seq(
            orderWith(amount = 50.waves, price = 0.49), // too low price
            orderWith(amount = 50.waves, price = 0.51), // isn't a multiple of the step price
            orderWith(amount = 50.waves, price = 100.01) // too high price
          ) foreach { order =>
            validateByAmountAndPrice(Map.empty) { order } shouldBe 'right
            validateByAmountAndPrice() { order } should produce("OrderInvalidPrice")
          }
        }
      }

      "matcherFee is too small according to rate for matcherFeeAssetId" in forAll(orderV3WithDynamicFeeSettingsAndRateCacheGen) {
        case (order, dynamicSettings, rates) =>
          validateByMatcherSettings(dynamicSettings, rateCache = rates)(order) shouldBe 'right

          val updatedRate = rates.getRate(order.matcherFeeAssetId).map(_ + 1).get
          rates.upsertRate(order.matcherFeeAssetId, updatedRate)

          validateByMatcherSettings(dynamicSettings, rateCache = rates)(order) should produce("FeeNotEnough")
      }

      "market order price is invalid or tradable balance is not enough for its execution" in {

        val enoughBalance = Map[Asset, Long](Waves -> 1000.waves, btc -> 10.btc, eth -> 500.eth)

        val orderBook =
          AggregatedSnapshot(
            asks = Seq(
              LevelAgg(amount = 32.waves, price = 0.00011842.btc), // buy part of level, value = 11 * 0.00011842 = 0.00130262.btc, remain to execute =  11 - 11 =  0
              LevelAgg(amount = 23.waves, price = 0.00011825.btc), // buy whole level,   value = 23 * 0.00011825 = 0.00271975.btc, remain to execute =  34 - 23 = 11
              LevelAgg(amount = 36.waves, price = 0.00011824.btc), // buy whole level,   value = 36 * 0.00011824 = 0.00425664.btc, remain to execute =  70 - 36 = 34
              LevelAgg(amount = 30.waves, price = 0.00011810.btc) //  buy whole level,   value = 30 * 0.00011810 = 0.00354300.btc, remain to execute = 100 - 30 = 70
            ).reverse,
            bids = Seq(
              LevelAgg(amount = 85.waves, price = 0.00011808.btc), // close part of level, value = 33.waves, remain to execute =  33 - 33 =  0
              LevelAgg(amount = 12.waves, price = 0.00011805.btc), // close whole level,   value = 12.waves, remain to execute =  45 - 12 = 33
              LevelAgg(amount = 40.waves, price = 0.00011787.btc), // close whole level,   value = 40.waves, remain to execute =  85 - 40 = 45
              LevelAgg(amount = 15.waves, price = 0.00011782.btc) //  close whole level,   value = 15.waves, remain to execute = 100 - 15 = 85
            )
          )

        // cost of 100.waves by the current market state = 0.00354300 + 0.00425664 + 0.00271975 + 0.00130262 = 0.01182201.btc

        val validateByTradableBalance: Map[Asset, Long] => Order => Result[AcceptedOrder] = validateMarketOrderByAccountStateAware { orderBook }
        val validateByPrice: Order => Result[AcceptedOrder]                               = validateByTradableBalance { enoughBalance }

        val buyAmount, sellAmount = 100.waves
        val (buyPrice, sellPrice) = (0.00011850, 0.00011750) // both prices are enough to collapse counter side

        withClue("BUY: in order to buy 100.waves price should be >= 0.00011842.btc, otherwise buy less\n") {
          validateByPrice { createOrder(pairWavesBtc, BUY, amount = 100.waves, price = 0.00011842) } shouldBe 'right                           // the lowest acceptable price for buying 100.waves
          validateByPrice { createOrder(pairWavesBtc, BUY, amount = 100.waves, price = 0.00011841) } should produce("InvalidMarketOrderPrice") // too low price (can only buy 30 + 36 + 23 = 89.waves)
        }

        withClue("SELL: in order to sell 100.waves price should be <= 0.00011787.btc, otherwise sell less\n") {
          validateByPrice { createOrder(pairWavesBtc, SELL, amount = 100.waves, price = 0.00011787) } shouldBe 'right                           // the highest acceptable price for selling 100.waves
          validateByPrice { createOrder(pairWavesBtc, SELL, amount = 100.waves, price = 0.00011788) } should produce("InvalidMarketOrderPrice") // too high price (can only sell 12 + 85 = 97.waves)
        }

        withClue("BUY: fee in received asset, required balance: Waves -> 0, BTC -> 0.01182201\n") {
          val marketOrder = createOrder(pairWavesBtc, BUY, buyAmount, buyPrice, matcherFeeAsset = Waves, matcherFee = 0.003.waves)
          validateByTradableBalance { Map(btc -> 0.01182201.btc) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182200.btc) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("BUY: fee in spent asset, required balance: Waves -> 0, BTC -> 0.01182201 + 0.00000035 = 0.01182236\n") {
          val marketOrder = createOrder(pairWavesBtc, BUY, buyAmount, buyPrice, matcherFeeAsset = btc, matcherFee = 0.00000035.btc)
          validateByTradableBalance { Map(btc -> 0.01182236.btc) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182235.btc) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("BUY: fee in third asset, required balance: Waves -> 0, BTC -> 0.01182201, ETH = 0.00649308\n") {
          val marketOrder = createOrder(pairWavesBtc, BUY, buyAmount, buyPrice, matcherFeeAsset = eth, matcherFee = 0.00649308.eth)
          validateByTradableBalance { Map(btc -> 0.01182201.btc, eth -> 0.00649308.eth) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182201.btc, eth -> 0.00649307.eth) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in received asset, required balance: Waves -> 100, BTC -> 0\n") {
          val marketOrder = createOrder(pairWavesBtc, SELL, sellAmount, sellPrice, matcherFeeAsset = btc, matcherFee = 0.00000035.btc)
          validateByTradableBalance { Map(Waves -> 100.00000000.waves) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 99.99999999.waves) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in spent asset, required balance: Waves -> 100.003, BTC -> 0\n") {
          val marketOrder = createOrder(pairWavesBtc, SELL, sellAmount, sellPrice, matcherFeeAsset = Waves, matcherFee = 0.003.waves)
          validateByTradableBalance { Map(Waves -> 100.00300000.waves) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 100.00299999.waves) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in third asset, required balance: Waves -> 100, BTC -> 0, ETH -> 0.00649308\n") {
          val marketOrder = createOrder(pairWavesBtc, SELL, sellAmount, sellPrice, matcherFeeAsset = eth, matcherFee = 0.00649308.eth)
          validateByTradableBalance { Map(Waves -> 100.waves, eth -> 0.00649308.eth) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 100.waves, eth -> 0.00649307.eth) }(marketOrder) should produce("BalanceNotEnough")
        }
      }
    }

    "verify script of matcherFeeAssetId" in {
      forAll(orderV3WithFeeSettingsGenerator) {
        case (order, orderFeeSettings) =>
          def setFeeAssetScriptAndValidate(matcherFeeAssetScript: Option[RunScriptResult]): Result[Order] =
            validateByBlockchain(orderFeeSettings)(None, None, matcherFeeAssetScript, None)(order)

          orderFeeSettings match {
            case _: FixedSettings =>
              setFeeAssetScriptAndValidate(Some(RunScriptResult.ScriptError("Some error"))) should produce("AssetScriptReturnedError")
              setFeeAssetScriptAndValidate(Some(RunScriptResult.Denied)) should produce("AssetScriptDeniedOrder")
              setFeeAssetScriptAndValidate(None) shouldBe 'right
            case _ =>
              // case _: FixedWavesSettings => it's impossible to set script for Waves
              // case _: PercentSettings    => matcherFeeAssetId script won't be validated since matcherFeeAssetId equals to one of the asset of the pair
              //                               (in that case additional validation of matcherFeeAssetId's script is not required)

              setFeeAssetScriptAndValidate(Some(RunScriptResult.ScriptError("Some error"))) shouldBe 'right
              setFeeAssetScriptAndValidate(Some(RunScriptResult.Denied)) shouldBe 'right
              setFeeAssetScriptAndValidate(None) shouldBe 'right
          }
      }
    }

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }

// @TODO run this to runScript
//
//    "meaningful error for undefined functions in matcher" in portfolioTest(defaultPortfolio) { (ov, bc) =>
//      activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
//
//      val pk     = KeyPair(randomBytes())
//      val o      = newBuyOrder(pk, version = 2)
//      val script = ScriptCompiler.compile("true && (height > 0)").explicitGet()._1
//      (bc.accountScript _).when(pk.toAddress).returns(Some(script))
//      ov(o).left.map(_.toJson(errorContext)) should produce("An access to the blockchain.height is denied on DEX")
//    }
//
//    "validate order with smart token" when {
//      val asset1 = mkAssetId("asset1")
//      val asset2 = mkAssetId("asset2")
//      val pair   = AssetPair(asset1, asset2)
//      val portfolio = Portfolio(10 * Constants.UnitsInWave,
//                                LeaseBalance.empty,
//                                Map(
//                                  asset1 -> 10 * Constants.UnitsInWave,
//                                  asset2 -> 10 * Constants.UnitsInWave
//                                ))
//
//      val permitScript = ExprScript(Terms.TRUE).explicitGet()
//      val denyScript   = ExprScript(Terms.FALSE).explicitGet()
//
//      "two assets are smart and they permit an order" when test { (ov, bc, o) =>
//        (bc.assetScript _).when(asset1).returns(Some(permitScript))
//        (bc.assetScript _).when(asset2).returns(Some(permitScript))
//
//        ov(o) shouldBe 'right
//      }
//
//      "first asset is smart and it deny an order" when test { (ov, bc, o) =>
//        (bc.assetScript _).when(asset1).returns(Some(denyScript))
//        (bc.assetScript _).when(asset2).returns(None)
//
//        ov(o) should produce("AssetScriptDeniedOrder")
//      }
//
//      "second asset is smart and it deny an order" when test { (ov, bc, o) =>
//        (bc.assetScript _).when(asset1).returns(None)
//        (bc.assetScript _).when(asset2).returns(Some(denyScript))
//
//        ov(o) should produce("AssetScriptDeniedOrder")
//      }
//
//      def test(f: (Order => OrderValidator.Result[Order], WavesBlockchainClient, Order) => Any): Unit = (1 to 2).foreach { version =>
//        s"v$version" in portfolioTest(portfolio) { (ov, bc) =>
//          val features = Seq(BlockchainFeatures.SmartAssets) ++ {
//            if (version == 1) Seq.empty else Seq(BlockchainFeatures.SmartAccountTrading)
//          }
//          activate(bc, features.contains(_))
//          assignAssetDescription(
//            bc,
//            asset1 -> mkAssetDescription(8),
//            asset2 -> mkAssetDescription(8)
//          )
//
//          val pk = KeyPair(randomBytes())
//          val o = buy(
//            pair = pair,
//            amount = 100 * Constants.UnitsInWave,
//            price = 0.0022,
//            sender = Some(pk),
//            matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
//            ts = Some(System.currentTimeMillis()),
//            version = version.toByte
//          )
//          assignNoScript(bc, o.sender.toAddress)
//          f(ov, bc, o)
//        }
//      }
//    }

    "deny OrderV2 if SmartAccountTrading hasn't been activated yet" in forAll(accountGen) { account =>
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        activate(bc, _ => false)
        assignScript(bc, account.toAddress, RunScriptResult.Allowed)

        ov(newBuyOrder(account, version = 2)) should produce("OrderVersionUnsupported")
      }
    }

// @TODO run this to runScript
//
//    "deny blockchain functions in account script" in forAll(accountGen) { account =>
//      portfolioTest(defaultPortfolio) { (ov, bc) =>
//        activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
//        (bc.height _).when().returns(0).anyNumberOfTimes()
//
//        val scriptText =
//          """match tx {
//            |  case o: Order => height >= 0
//            |  case _ => true
//            |}""".stripMargin
//        val script = ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
//        (bc.accountScript _).when(account.toAddress).returns(Some(script)).anyNumberOfTimes()
//
//        ov(newBuyOrder(account, version = 2)).left.map(_.toJson(errorContext)) should produce("An access to the blockchain.height is denied on DEX")
//      }
//    }
  }

  private def portfolioTest(p: Portfolio)(f: (Order => OrderValidator.Result[Order], WavesBlockchainClient) => Any): Unit = {
    val bc = stub[WavesBlockchainClient]
    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    f(ov, bc)
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = {
    val bc = stub[WavesBlockchainClient]
    val pk = KeyPair(randomBytes())

    activate(bc, _ == BlockchainFeatures.SmartAccountTrading.id)
    assignScript(bc, pk.toAddress, RunScriptResult.Allowed)
    assignNoScript(bc, btc)
    assignAssetDescription(bc, btc -> mkAssetDescription(8))

    val order = OrderV2(
      senderPublicKey = pk,
      matcherPublicKey = MatcherAccount,
      assetPair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = (0.0022 * Order.PriceConstant).toLong,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 60 * 60 * 1000L,
      matcherFee = (0.003 * Constants.UnitsInWave).toLong,
      orderType = BUY,
      proofs = Proofs.empty
    )

    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    ov(order) shouldBe 'right
  }

  private def mkAssetDescription(decimals: Int): BriefAssetDescription =
    BriefAssetDescription(name = "name".getBytes(StandardCharsets.UTF_8), decimals = decimals, hasScript = false)

  private def newBuyOrder: Order =
    buy(pair = pairWavesBtc, amount = 100 * Constants.UnitsInWave, price = 0.0022, matcherFee = Some((0.003 * Constants.UnitsInWave).toLong))

  private def newBuyOrder(pk: KeyPair, ts: Long = 0, version: Byte = 1) =
    buy(
      pair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = 0.0022,
      sender = Some(pk),
      matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
      ts = Some(ts),
      version = version
    )

//  private def activate(bc: WavesBlockchainClient, isActive: PartialFunction[Short, Boolean]): Unit =
//    activate(bc, isActive.lift(_).getOrElse(false))

  private def activate(bc: WavesBlockchainClient, isActive: Function[Short, Boolean]): Unit =
    (bc.isFeatureActivated _).when(*).onCall(isActive)

  private def mkOrderValidator(bc: WavesBlockchainClient, tc: ExchangeTransactionCreator) =
    OrderValidator.blockchainAware(bc,
                                   tc.createTransaction,
                                   MatcherAccount,
                                   ntpTime,
                                   matcherSettings.orderFee,
                                   matcherSettings.orderRestrictions,
                                   rateCache,
                                   getDefaultAssetDecimals)(_)

  private def tradableBalance(p: Portfolio)(assetId: Asset): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: WavesBlockchainClient) =
    new ExchangeTransactionCreator(MatcherAccount,
                                   matcherSettings,
                                   blockchain.hasScript(MatcherAccount),
                                   blockchain.hasScript(_),
                                   blockchain.isFeatureActivated)

  private def asa[A](
      p: Portfolio = defaultPortfolio,
      orderStatus: ByteStr => Boolean = _ => false,
      o: Order = newBuyOrder
  )(f: OrderValidator.Result[AcceptedOrder] => A): A =
    f(OrderValidator.accountStateAware(o.sender, tradableBalance(p), 0, orderStatus, _ => OrderBook.AggregatedSnapshot())(LimitOrder(o)))

  private def validateMarketOrderByAccountStateAware(aggregatedSnapshot: AggregatedSnapshot)(b: Map[Asset, Long]): Order => Result[AcceptedOrder] = {
    order =>
      OrderValidator.accountStateAware(
        sender = order.sender.toAddress,
        tradableBalance = b.withDefaultValue(0L).apply,
        activeOrderCount = 0,
        orderExists = _ => false,
        orderBookCache = _ => aggregatedSnapshot,
      ) { MarketOrder(order, b.apply _) }
  }

  private def msa(ba: Set[Address], o: Order) =
    OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, Set.empty, matcherSettings, rateCache, getDefaultAssetDecimals) _

  private def validateByMatcherSettings(orderFeeSettings: OrderFeeSettings,
                                        blacklistedAssets: Set[IssuedAsset] = Set.empty[IssuedAsset],
                                        allowedAssetPairs: Set[AssetPair] = Set.empty[AssetPair],
                                        allowedOrderVersions: Set[Byte] = Set(1, 2, 3),
                                        rateCache: RateCache = rateCache,
                                        assetDecimals: Asset => Int = getDefaultAssetDecimals): Order => Result[Order] =
    order =>
      OrderValidator
        .matcherSettingsAware(
          MatcherAccount,
          Set.empty,
          blacklistedAssets,
          matcherSettings.copy(orderFee = orderFeeSettings, allowedAssetPairs = allowedAssetPairs, allowedOrderVersions = allowedOrderVersions),
          rateCache,
          assetDecimals
        )(order)

  private def validateByBlockchain(orderFeeSettings: OrderFeeSettings,
                                   orderRestrictions: Map[AssetPair, OrderRestrictionsSettings] = matcherSettings.orderRestrictions)(
      amountAssetScript: Option[RunScriptResult] = None,
      priceAssetScript: Option[RunScriptResult] = None,
      matcherFeeAssetScript: Option[RunScriptResult] = None,
      matcherAccountScript: Option[RunScriptResult] = None,
      assetDecimals: Asset => Int = getDefaultAssetDecimals,
      rateCache: RateCache = rateCache)(order: Order): OrderValidator.Result[Order] = {

    val blockchain = stub[WavesBlockchainClient]

    activate(
      blockchain,
      List(
        BlockchainFeatures.SmartAccountTrading,
        BlockchainFeatures.OrderV3,
        BlockchainFeatures.SmartAssets
      ).map(_.id).contains(_)
    )

    def prepareAssets(assetsAndScripts: (Asset, Option[RunScriptResult], Int)*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption, decimals) =>
        assignAssetDescription(blockchain, asset -> mkAssetDescription(decimals))
        assignScript(blockchain, asset, scriptOption)
      case _ =>
    }

    prepareAssets(
      (order.assetPair.amountAsset, amountAssetScript, assetDecimals(order.assetPair.amountAsset)),
      (order.assetPair.priceAsset, priceAssetScript, assetDecimals(order.assetPair.priceAsset)),
      (order.matcherFeeAssetId, matcherFeeAssetScript, assetDecimals(order.matcherFeeAssetId))
    )

    assignScript(blockchain, MatcherAccount.toAddress, matcherAccountScript)
    assignNoScript(blockchain, order.sender.toAddress)

    val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

    OrderValidator
      .blockchainAware(blockchain,
                       transactionCreator,
                       MatcherAccount.toAddress,
                       ntpTime,
                       orderFeeSettings,
                       orderRestrictions,
                       rateCache,
                       assetDecimals)(order)
  }

  private def assignScript(bc: WavesBlockchainClient, address: Address, result: RunScriptResult): Unit = {
    (bc.hasScript(_: Address)).when(address).returns(true)
    (bc.runScript(_: Address, _: Order)).when(address, *).onCall((_, _) => result)
  }

  private def assignScript(bc: WavesBlockchainClient, address: Address, result: Option[RunScriptResult]): Unit = result match {
    case None => (bc.hasScript(_: Address)).when(address).returns(false)
    case Some(r) =>
      (bc.hasScript(_: Address)).when(address).returns(true)
      (bc.runScript(_: Address, _: Order)).when(address, *).onCall((_, _) => r)
  }

  private def assignScript(bc: WavesBlockchainClient, asset: IssuedAsset, result: Option[RunScriptResult]): Unit = result match {
    case None => (bc.hasScript(_: IssuedAsset)).when(asset).returns(false)
    case Some(r) =>
      (bc.hasScript(_: IssuedAsset)).when(asset).returns(true)
      (bc.runScript(_: IssuedAsset, _: ExchangeTransaction)).when(asset, *).onCall((_, _) => r)
  }

  private def assignNoScript(bc: WavesBlockchainClient, address: Address): Unit =
    (bc.hasScript(_: Address)).when(address).returns(false)

  private def assignNoScript(bc: WavesBlockchainClient, asset: IssuedAsset): Unit =
    (bc.hasScript(_: IssuedAsset)).when(asset).returns(false)

  private def assignAssetDescription(bc: WavesBlockchainClient, xs: (IssuedAsset, BriefAssetDescription)*): Unit =
    xs.foreach {
      case (asset, desc) =>
        (bc.assetDescription _).when(asset).onCall { (x: IssuedAsset) =>
          Some(desc)
        }
    }
}
