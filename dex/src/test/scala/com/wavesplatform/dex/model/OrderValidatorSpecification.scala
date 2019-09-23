package com.wavesplatform.dex.model

import java.util.concurrent.ConcurrentHashMap

import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderValidator.Result
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, FixedSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, DeviationsSettings, OrderRestrictionsSettings}
import com.wavesplatform.dex.{MatcherTestData, RateCache}
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AssetDescription, Blockchain, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.{NoShrink, TestTime, WithDB}
import mouse.any._
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

  private val btc           = mkAssetId("WBTC")
  private val usd           = mkAssetId("WUSD")
  private val senderKeyPair = KeyPair("seed".getBytes("utf-8"))

  override val getDefaultAssetDecimals: Asset => Int = Map[Asset, Int](usd -> 2, btc -> 8).withDefaultValue(defaultAssetDecimals).apply _
  override val rateCache: RateCache                  = RateCache.inMem unsafeTap { _.upsertRate(usd, 3.7) } unsafeTap { _.upsertRate(btc, 0.00011167) }

  private val pairWavesBtc = AssetPair(Waves, btc)
  private val pairWavesUsd = AssetPair(Waves, usd)

  private lazy val accountScript = ExprScript(V2, Terms.TRUE, checkSize = false).explicitGet()
  private val defaultPortfolio   = Portfolio(0, LeaseBalance.empty, Map(btc -> 10 * Constants.UnitsInWave))

  private implicit val errorContext: ErrorFormatterContext = _ => defaultAssetDecimals

  implicit class DoubleOps(value: Double) {
    val waves, btc = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long  = Normalization.normalizeAmountAndFee(value, 2)
  }

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
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).once()

          ov(newBuyOrder(scripted)) should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).anyNumberOfTimes()

          ov(newBuyOrder(scripted)) should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.FALSE).explicitGet()))
          (bc.height _).when().returns(150).anyNumberOfTimes()

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
        (bc.accountScript _).when(pk.toAddress).returns(None)
        val order = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
          case x: OrderV2 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
        }
        ov(order) should produce("InvalidSignature")
      }

      "order exists" in {
        val pk = KeyPair(randomBytes())
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true)(_)
        ov(newBuyOrder(pk, 1000)) should produce("OrderDuplicate")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetIdGen(1), accountGen, Gen.choose(1, 7)) {
        case (amountAsset, sender, amountDecimals) =>
          portfolioTest(Portfolio(11 * Constants.UnitsInWave, LeaseBalance.empty, Map.empty)) { (ov, bc) =>
            (bc.hasScript _).when(sender.toAddress).returns(false)
            (bc.assetDescription _).when(amountAsset).returns(mkAssetDescription(amountDecimals))

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

        def validateFeeByBlockchain(priceAssetScript: Option[Script] = None, matcherScript: Option[Script] = None): Order => Result[Order] = {
          validateByBlockchain { DynamicSettings(0.003.waves) }(priceAssetScript = priceAssetScript,
                                                                matcherAccountScript = matcherScript,
                                                                rateCache = rateCache)
        }

        def updateOrder(order: Order, f: Order => Order): Order = Order.sign(f(order), senderKeyPair)

        withClue(s"Fee in USD (2 decimals, rate = 3.7) ") {

          val order  = createOrder(pairWavesUsd, OrderType.BUY, 1.waves, price = 3.7, matcherFee = 0.02.usd, matcherFeeAsset = usd)
          val script = Some { ExprScript(Terms.TRUE).explicitGet() }

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
          val trueScript = ExprScript(Terms.TRUE).explicitGet()

          def setAssetsAndMatcherAccountScriptsAndValidate(amountAssetScript: Option[Script],
                                                           priceAssetScript: Option[Script],
                                                           matcherAccountScript: Option[Script]): Result[Order] =
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

      "buy order's price is too high (market aware)" in {
        val preconditions =
          for {
            bestAmount <- maxWavesAmountGen
            bestPrice  <- Gen.choose(1, (Long.MaxValue / bestAmount) - 100)

            bestAsk                = LevelAgg(bestAmount, bestPrice)
            deviationSettings      = DeviationsSettings(enabled = true, 50, 70, 50)
            tooHighPriceInBuyOrder = (bestAsk.price * (1 + (deviationSettings.maxPriceLoss / 100))).toLong + 50L

            (order, orderFeeSettings) <- orderWithFeeSettingsGenerator(OrderType.BUY, tooHighPriceInBuyOrder)
          } yield {
            val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
            assetPair2MarketStatus.put(order.assetPair, MarketStatus(None, None, Some(bestAsk)))
            (order, orderFeeSettings, deviationSettings, Option(assetPair2MarketStatus.get(order.assetPair)))
          }

        forAll(preconditions) {
          case (order, orderFeeSettings, deviationSettings, nonEmptyMarketStatus) =>
            OrderValidator.marketAware(orderFeeSettings, deviationSettings, nonEmptyMarketStatus, rateCache)(order) should produce(
              "DeviantOrderPrice")
        }
      }

      "sell order's price is out of deviation bounds (market aware)" in {
        val fixedWavesFeeSettings = DynamicSettings(300000L)

        // seller cannot sell with price which:
        //   1. less than 50% of best bid (sell order price must be >= 2000)
        //   2. higher than 170% of best ask (sell order price must be <= 8500)

        val deviationSettings = DeviationsSettings(true, maxPriceProfit = 70, maxPriceLoss = 50, 50)
        val bestBid           = LevelAgg(1000L, 4000L)
        val bestAsk           = LevelAgg(1000L, 5000L)

        val tooLowPrice      = 1999L // = 50% of best bid (4000) - 1, hence invalid
        val lowButValidPrice = 2000L // = 50% of best bid (4000)

        val tooHighPrice      = 8501L // = 170% of best ask (5000) + 1, hence invalid
        val highButValidPrice = 8500L // = 170% of best ask (5000)

        val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
        assetPair2MarketStatus.put(pairWavesBtc, MarketStatus(None, Some(bestBid), Some(bestAsk)))
        val nonEmptyMarketStatus = assetPair2MarketStatus.get(pairWavesBtc)

        val tooLowPriceOrder =
          Order(
            sender = KeyPair("seed".getBytes("utf-8")),
            matcher = MatcherAccount,
            pair = pairWavesBtc,
            orderType = OrderType.SELL,
            amount = 1000,
            price = tooLowPrice,
            timestamp = System.currentTimeMillis() - 10000L,
            expiration = System.currentTimeMillis() + 10000L,
            matcherFee = 1000L,
            version = 3: Byte,
            matcherFeeAssetId = Waves
          )

        val lowButValidPriceOrder  = tooLowPriceOrder.updatePrice(lowButValidPrice)
        val tooHighPriceOrder      = tooLowPriceOrder.updatePrice(tooHighPrice)
        val highButValidPriceOrder = tooLowPriceOrder.updatePrice(highButValidPrice)

        val orderValidator = OrderValidator.marketAware(fixedWavesFeeSettings, deviationSettings, Option(nonEmptyMarketStatus), rateCache) _

        orderValidator(tooLowPriceOrder) should produce("DeviantOrderPrice")
        orderValidator(lowButValidPriceOrder) shouldBe 'right

        orderValidator(tooHighPriceOrder) should produce("DeviantOrderPrice")
        orderValidator(highButValidPriceOrder) shouldBe 'right
      }

      "order's fee is out of deviation bounds (market aware)" in {
        val percentSettings   = PercentSettings(AssetType.PRICE, 10)
        val deviationSettings = DeviationsSettings(true, 100, 100, maxFeeDeviation = 10)

        val bestAsk = LevelAgg(1000L, 4000L)

        val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
        assetPair2MarketStatus.put(pairWavesBtc, MarketStatus(None, None, Some(bestAsk)))
        val nonEmptyMarketStatus = assetPair2MarketStatus.get(pairWavesBtc)

        val order =
          Order(
            sender = KeyPair("seed".getBytes("utf-8")),
            matcher = MatcherAccount,
            pair = pairWavesBtc,
            orderType = OrderType.BUY,
            amount = 1000,
            price = 1000,
            timestamp = System.currentTimeMillis() - 10000L,
            expiration = System.currentTimeMillis() + 10000L,
            matcherFee = 1000L,
            version = 3: Byte,
            matcherFeeAssetId = btc
          )

        val validFee =
          OrderValidator
            .getMinValidFeeForSettings(order,
                                       percentSettings,
                                       bestAsk.price,
                                       rateCache,
                                       getDefaultAssetDecimals,
                                       1 - (deviationSettings.maxFeeDeviation / 100))

        val validOrder   = order.updateFee(validFee)
        val invalidOrder = order.updateFee(validFee - 1L)

        val orderValidator = OrderValidator.marketAware(percentSettings, deviationSettings, Option(nonEmptyMarketStatus), rateCache) _

        orderValidator(invalidOrder) should produce("DeviantOrderMatcherFee")
        orderValidator(validOrder) shouldBe 'right
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
    }

    "verify script of matcherFeeAssetId" in {
      forAll(orderV3WithFeeSettingsGenerator) {
        case (order, orderFeeSettings) =>
          def setFeeAssetScriptAndValidate(matcherFeeAssetScript: Option[Script]): Result[Order] =
            validateByBlockchain(orderFeeSettings)(None, None, matcherFeeAssetScript, None)(order)

          val (invalidScript, _) = ScriptCompiler.compile("(5 / 0) == 2", ScriptEstimatorV2).explicitGet()
          val falseScript        = ExprScript(Terms.FALSE).explicitGet()

          orderFeeSettings match {
            case _: FixedSettings =>
              setFeeAssetScriptAndValidate(Some(invalidScript)) should produce("AssetScriptReturnedError")
              setFeeAssetScriptAndValidate(Some(falseScript)) should produce("AssetScriptDeniedOrder")
              setFeeAssetScriptAndValidate(None) shouldBe 'right
            case _ =>
              // case _: FixedWavesSettings => it's impossible to set script for Waves
              // case _: PercentSettings    => matcherFeeAssetId script won't be validated since matcherFeeAssetId equals to one of the asset of the pair
              //                               (in that case additional validation of matcherFeeAssetId's script is not required)

              setFeeAssetScriptAndValidate(Some(invalidScript)) shouldBe 'right
              setFeeAssetScriptAndValidate(Some(falseScript)) shouldBe 'right
              setFeeAssetScriptAndValidate(None) shouldBe 'right
          }
      }
    }

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }

    "meaningful error for undefined functions in matcher" in portfolioTest(defaultPortfolio) { (ov, bc) =>
      activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)

      val pk     = KeyPair(randomBytes())
      val o      = newBuyOrder(pk, version = 2)
      val script = ScriptCompiler("true && (height > 0)", isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
      (bc.accountScript _).when(pk.toAddress).returns(Some(script))
      ov(o).left.map(_.toJson(errorContext)) should produce("An access to the blockchain.height is denied on DEX")
    }

    "validate order with smart token" when {
      val asset1 = mkAssetId("asset1")
      val asset2 = mkAssetId("asset2")
      val pair   = AssetPair(asset1, asset2)
      val portfolio = Portfolio(10 * Constants.UnitsInWave,
                                LeaseBalance.empty,
                                Map(
                                  asset1 -> 10 * Constants.UnitsInWave,
                                  asset2 -> 10 * Constants.UnitsInWave
                                ))

      val permitScript = ExprScript(Terms.TRUE).explicitGet()
      val denyScript   = ExprScript(Terms.FALSE).explicitGet()

      "two assets are smart and they permit an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(Some(permitScript))
        (bc.assetScript _).when(asset2).returns(Some(permitScript))

        ov(o) shouldBe 'right
      }

      "first asset is smart and it deny an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(Some(denyScript))
        (bc.assetScript _).when(asset2).returns(None)

        ov(o) should produce("AssetScriptDeniedOrder")
      }

      "second asset is smart and it deny an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(None)
        (bc.assetScript _).when(asset2).returns(Some(denyScript))

        ov(o) should produce("AssetScriptDeniedOrder")
      }

      def test(f: (Order => OrderValidator.Result[Order], Blockchain, Order) => Any): Unit = (1 to 2).foreach { version =>
        s"v$version" in portfolioTest(portfolio) { (ov, bc) =>
          val features = Seq(BlockchainFeatures.SmartAssets -> 0) ++ {
            if (version == 1) Seq.empty
            else Seq(BlockchainFeatures.SmartAccountTrading -> 0)
          }
          activate(bc, features: _*)
          (bc.assetDescription _).when(asset1).returns(mkAssetDescription(8))
          (bc.assetDescription _).when(asset2).returns(mkAssetDescription(8))

          val pk = KeyPair(randomBytes())
          val o = buy(
            pair = pair,
            amount = 100 * Constants.UnitsInWave,
            price = 0.0022,
            sender = Some(pk),
            matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
            ts = Some(System.currentTimeMillis()),
            version = version.toByte
          )
          (bc.accountScript _).when(o.sender.toAddress).returns(None)
          f(ov, bc, o)
        }
      }
    }

    "deny OrderV2 if SmartAccountTrading hasn't been activated yet" in forAll(accountGen) { account =>
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
        (bc.accountScript _).when(account.toAddress).returns(Some(accountScript)).anyNumberOfTimes()
        (bc.height _).when().returns(0).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)) should produce("OrderVersionUnsupported")
      }
    }

    "deny blockchain functions in account script" in forAll(accountGen) { account =>
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
        (bc.height _).when().returns(0).anyNumberOfTimes()

        val scriptText =
          """match tx {
            |  case o: Order => height >= 0
            |  case _ => true
            |}""".stripMargin
        val script = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
        (bc.accountScript _).when(account.toAddress).returns(Some(script)).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)).left.map(_.toJson(errorContext)) should produce("An access to the blockchain.height is denied on DEX")
      }
    }
  }

  private def portfolioTest(p: Portfolio)(f: (Order => OrderValidator.Result[Order], Blockchain) => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetScript _).when(btc).returns(None)
    (bc.assetDescription _).when(btc).returns(mkAssetDescription(8)).anyNumberOfTimes()
    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    f(ov, bc)
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = {
    val bc = stub[Blockchain]
    val pk = KeyPair(randomBytes())

    activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
    (bc.accountScript _).when(pk.toAddress).returns(Some(accountScript)).anyNumberOfTimes()
    (bc.height _).when().returns(1).anyNumberOfTimes()
    (bc.assetScript _).when(btc).returns(None)
    (bc.assetDescription _).when(btc).returns(mkAssetDescription(8)).anyNumberOfTimes()

    val order = OrderV2(
      senderPublicKey = pk,
      matcherPublicKey = MatcherAccount,
      assetPair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = (0.0022 * Order.PriceConstant).toLong,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 60 * 60 * 1000L,
      matcherFee = (0.003 * Constants.UnitsInWave).toLong,
      orderType = OrderType.BUY,
      proofs = Proofs.empty
    )

    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    ov(order) shouldBe 'right
  }

  private def mkAssetDescription(decimals: Int): Option[AssetDescription] =
    Some(AssetDescription(MatcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0))

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

  private def activate(bc: Blockchain, features: (BlockchainFeature, Int)*): Unit = {
    (bc.activatedFeatures _).when().returns(features.map(x => x._1.id -> x._2).toMap).anyNumberOfTimes()
  }

  private def mkOrderValidator(bc: Blockchain, tc: ExchangeTransactionCreator) =
    OrderValidator.blockchainAware(bc,
                                   tc.createTransaction,
                                   MatcherAccount,
                                   ntpTime,
                                   matcherSettings.orderFee,
                                   matcherSettings.orderRestrictions,
                                   rateCache,
                                   getDefaultAssetDecimals)(_)

  private def tradableBalance(p: Portfolio)(assetId: Asset): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: Blockchain) = new ExchangeTransactionCreator(blockchain, MatcherAccount, matcherSettings)

  private def asa[A](
      p: Portfolio = defaultPortfolio,
      orderStatus: ByteStr => Boolean = _ => false,
      o: Order = newBuyOrder
  )(f: OrderValidator.Result[Order] => A): A =
    f(OrderValidator.accountStateAware(o.sender, tradableBalance(p), 0, orderStatus)(o))

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
      amountAssetScript: Option[Script] = None,
      priceAssetScript: Option[Script] = None,
      matcherFeeAssetScript: Option[Script] = None,
      matcherAccountScript: Option[Script] = None,
      assetDecimals: Asset => Int = getDefaultAssetDecimals,
      rateCache: RateCache = rateCache)(order: Order): OrderValidator.Result[Order] = {

    val blockchain = stub[Blockchain]

    activate(blockchain, BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0, BlockchainFeatures.SmartAssets -> 0)

    def prepareAssets(assetsAndScripts: (Asset, Option[Script], Int)*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption, decimals) =>
        (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(decimals))
        (blockchain.assetScript _).when(asset).returns(scriptOption)
        (blockchain.hasAssetScript _).when(asset).returns(scriptOption.isDefined)
      case _ =>
    }

    prepareAssets(
      (order.assetPair.amountAsset, amountAssetScript, assetDecimals(order.assetPair.amountAsset)),
      (order.assetPair.priceAsset, priceAssetScript, assetDecimals(order.assetPair.priceAsset)),
      (order.matcherFeeAssetId, matcherFeeAssetScript, assetDecimals(order.matcherFeeAssetId))
    )

    (blockchain.accountScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript)
    (blockchain.hasScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript.isDefined)

    (blockchain.accountScript _).when(order.sender.toAddress).returns(None)
    (blockchain.hasScript _).when(order.sender.toAddress).returns(false)

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

  private def createOrder(pair: AssetPair,
                          orderType: OrderType,
                          amount: Long,
                          price: Double,
                          matcherFee: Long = 0.003.waves,
                          version: Byte = 3,
                          matcherFeeAsset: Asset = Waves): Order = {
    Order(
      sender = senderKeyPair,
      matcher = MatcherAccount,
      pair = pair,
      orderType = orderType,
      amount = amount,
      price = Normalization.normalizePrice(price, getDefaultAssetDecimals(pair.amountAsset), getDefaultAssetDecimals(pair.priceAsset)),
      timestamp = ntpNow,
      expiration = ntpNow + (1000 * 60 * 60 * 24),
      matcherFee = matcherFee,
      version = version,
      matcherFeeAssetId = matcherFeeAsset
    )
  }
}
