package com.wavesplatform.dex.model

import cats.implicits._
import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.cache.RateCache
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.{Denormalization, Normalization}
import com.wavesplatform.dex.model.OrderBook.AggregatedSnapshot
import com.wavesplatform.dex.model.OrderValidator.Result
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, FixedSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, DeviationsSettings, OrderRestrictionsSettings}
import com.wavesplatform.dex.{AssetPairDecimals, MatcherTestData}
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
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

  private val wbtc               = mkAssetId("WBTC")
  private val pairWavesBtc       = AssetPair(Waves, wbtc)
  private lazy val accountScript = ExprScript(V2, Terms.TRUE, checkSize = false).explicitGet()

  private val defaultPortfolio          = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))
  private val defaultAssetDecimals: Int = 8

  private implicit val errorContext: ErrorFormatterContext = (asset: Asset) => defaultAssetDecimals

  val pairDecimals = new AssetPairDecimals(defaultAssetDecimals.toByte, defaultAssetDecimals.toByte)

  implicit class DoubleOps(value: Double) {
    val waves: Long = pairDecimals.amount(value)
    val btc: Long   = pairDecimals.price(value)
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
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true, _ => OrderBook.AggregatedSnapshot())(_)
        ov(LimitOrder(newBuyOrder(pk, 1000))) should produce("OrderDuplicate")
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

      "matcherFee is too small (percent mode)" in {
        def setFeeLessThanMinBy(percentSettings: PercentSettings)(order: Order): Order = {
          order
            .updateFee(OrderValidator.getMinValidFeeForSettings(order, percentSettings, order.price, rateCache) - 1)
            .updateMatcherFeeAssetId(OrderValidator.getValidFeeAssetForSettings(order, percentSettings, rateCache).head)
        }

        val preconditions =
          for {
            percentFeeSettings <- percentSettingsGenerator
            order              <- orderV3Generator map setFeeLessThanMinBy(percentFeeSettings) // incorrect fee (less than minimal admissible by 1) but correct asset
          } yield (order, percentFeeSettings)

        forAll(preconditions) {
          case (order, percentFeeSettings) => validateByMatcherSettings(percentFeeSettings)(order) should produce("FeeNotEnough")
        }
      }

      "matcherFee is too small (fixed mode)" in forAll(orderV3Generator) { order =>
        validateByMatcherSettings(FixedSettings(order.matcherFeeAssetId, order.matcherFee + 1))(order) should produce("FeeNotEnough")
      }

      "matcherFee is too small (dynamic mode)" in forAll(orderV3WithPredefinedFeeAssetGenerator(Some(Waves))) {
        case (_, order) => validateByMatcherSettings(DynamicSettings(order.matcherFee + 1))(order) should produce("FeeNotEnough")
      }

      "matcherFee is less than calculated by ExchangeTransactionCreator one" in forAll(orderWithFeeSettingsGenerator) {
        case (order, sender, orderFeeSettings) =>
          val baseFee = orderFeeSettings match {
            case DynamicSettings(fee) => fee
            case _                    => matcherSettings.exchangeTxBaseFee
          }

          val orderValidator = setScriptsAndValidate(orderFeeSettings)(None, None, None, None) _ // assets and accounts don't have any scripts
          val minFee         = ExchangeTransactionCreator.minFee(stub[Blockchain], MatcherAccount, order.assetPair, baseFee)
          val correctedOrder = Order.sign(order.updateFee(minFee - 1), sender)

          orderFeeSettings match {
            case _: DynamicSettings => orderValidator(correctedOrder) should produce("FeeNotEnough")
            case _                  => orderValidator(correctedOrder) shouldBe 'right
          }
      }

      "matcherFee is insufficient in case of scripted account or asset" in forAll(orderWithoutWavesInPairAndWithFeeSettingsGenerator) {
        case (order, _, orderFeeSettings) =>
          val trueScript = ExprScript(Terms.TRUE).explicitGet()

          def setAssetsAndMatcherAccountScriptsAndValidate(amountAssetScript: Option[Script],
                                                           priceAssetScript: Option[Script],
                                                           matcherAccountScript: Option[Script]): Result[Order] =
            setScriptsAndValidate(orderFeeSettings)(amountAssetScript, priceAssetScript, None, matcherAccountScript)(order)

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

        val bestAsk = LevelAgg(amount = 800.waves, price = 0.00011082.btc)
        val bestBid = LevelAgg(amount = 600.waves, price = 0.00011080.btc)

        val nonEmptyMarketStatus = MarketStatus(None, Some(bestBid), Some(bestAsk))
        val orderFeeSettings     = DynamicSettings(0.003.waves)
        val priceValidation      = OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(nonEmptyMarketStatus), rateCache) _

        val buyOrder  = createOrder(OrderType.BUY, amount = 250.waves, price = 0.00011081.btc)
        val sellOrder = createOrder(OrderType.SELL, amount = 250.waves, price = 0.00011081.btc)

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
        priceValidation { buyOrder } shouldBe 'right

        withClue("buy order price should be >= 0.5 * best bid = 0.5 * 0.00011080.btc = 0.00005540.btc\n") {
          Array(0, 0.00000001, 0.00000033, 0.00000450, 0.0000045, 0.00002770, 0.0000277, 0.00005539).foreach(price =>
            priceValidation { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }

        withClue("0.5 * best bid <= buy order price <= 1.7 * best ask (0.00005540.btc <= price <= 0.00018839.btc)\n") {
          Array(0.00005540, 0.00005541, 0.00005580, 0.00005641, 0.00006, 0.0001, 0.00017999, 0.00018799, 0.00018829, 0.00018838, 0.00018839).foreach(price =>
            priceValidation { buyOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        withClue("buy order price should be <= 1.7 * best ask = 1.7 * 0.00011082.btc = 0.00018839.btc\n") {
          Array(0.00018840, 0.00018841, 0.00037678, 0.00123456, 0.01951753, 0.98745612, 1, 1.12345678, 5000.12341234, 100000.1, 100000.1234, 100000.12347894).foreach(price =>
            priceValidation { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))
        }

        priceValidation { sellOrder } shouldBe 'right

        withClue("sell order price should be >= 0.3 * best bid = 0.3 * 0.00011080.btc = 0.00003324.btc\n") {
        Array(0, 0.00000001, 0.00000011, 0.000015, 0.00003322, 0.00002999, 0.00003299, 0.00003319, 0.00003323).foreach(price =>
            priceValidation { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice")
          )
        }

        withClue("0.3 * best bid <= sell order price <= 1.5 * best ask (0.00003324.btc <= price <= 0.00016623.btc)\n") {
          Array(0.00003324, 0.00003325, 0.00003999, 0.00006648, 0.00016622, 0.00016623).foreach(price =>
            priceValidation { sellOrder.updatePrice(price.btc) } shouldBe 'right
          )
        }

        withClue("sell order price should be <= 1.5 * best ask = 1.5 * 0.00011082.btc = 0.00016623.btc\n") {
          Array(0.00016624, 0.0001671, 0.00016633, 0.000167, 0.00017, 0.00033248, 0.0009, 0.00123123, 0.12312311, 1.12312312, 123.12312123, 100000.1, 100000.123123).foreach(price =>
            priceValidation { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice")
          )
        }
      }

      "order's fee is out of deviation bounds (market aware)" in {

        val percentSettings   = PercentSettings(AssetType.PRICE, 1)                                // matcher fee = 1% of the deal
        val deviationSettings = DeviationsSettings(enabled = true, 100, 100, maxFeeDeviation = 10) // fee deviation = 10%

        val bestAsk = LevelAgg(amount = 800.waves, price = 0.00011082.btc)
        val bestBid = LevelAgg(amount = 600.waves, price = 0.00011080.btc)

        val nonEmptyMarketStatus = MarketStatus(None, Some(bestBid), Some(bestAsk))
        val feeValidation        = OrderValidator.marketAware(percentSettings, deviationSettings, Some(nonEmptyMarketStatus), rateCache) _

        // matherFee = 1% of (amount * price) = 0.000277025 => 0.00027702
        val buyOrder  = createOrder(OrderType.BUY, amount = 250.waves, price = 0.00011081.btc, matcherFee = 0.00027702.btc, matcherFeeAsset = wbtc)
        val sellOrder = createOrder(OrderType.SELL, amount = 250.waves, price = 0.00011081.btc, matcherFee = 0.00027702.btc, matcherFeeAsset = wbtc)

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
        feeValidation { buyOrder } shouldBe 'right

        withClue("buy order fee should be >= 0.01 * 0.9 * best ask * amount = 0.01 * 0.9 * 0.00011082.btc * 250 = 0.00024935.btc\n") {
          Array(0, 0.00000001, 0.00001, 0.0001, 0.00012467, 0.00019999, 0.00023999, 0.00024899, 0.00024929, 0.00024934).foreach(fee =>
            feeValidation { buyOrder.updateFee(fee.btc) } should produce("DeviantOrderMatcherFee")
          )
        }

        withClue("buy order fee >= 0.01 * 0.9 * best ask * amount = 0.01 * 0.9 * 0.00011082.btc * 250 = 0.00024935.btc\n") {
          Array(0.00024935, 0.00024936, 0.0002494, 0.00025001, 0.0003, 0.00123123, 1.1231231, 123123.1, 123123.12312312).foreach(fee =>
            feeValidation { buyOrder.updateFee(fee.btc) } shouldBe 'right
          )
        }

        feeValidation { sellOrder } shouldBe 'right

        withClue("sell order fee should be >= 0.01 * 0.9 * best bid * amount = 0.01 * 0.9 * 0.00011080.btc * 250 = 0.00024930.btc\n") {
          Array(0, 0.00000001, 0.00001, 0.0001, 0.00012467, 0.00019999, 0.00023999, 0.00024899, 0.00024929).foreach(fee =>
            feeValidation { sellOrder.updateFee(fee.btc) } should produce("DeviantOrderMatcherFee")
          )
        }

        withClue("sell order fee >= 0.01 * 0.9 * best bid * amount = 0.01 * 0.9 * 0.00011080.btc * 250 = 0.00024930.btc\n") {
          Array(0.00024930, 0.00024931, 0.00024940, 0.00025, 0.0003, 0.00123123, 1.1231231, 123123.1, 123123.12312312).foreach(fee =>
            feeValidation { sellOrder.updateFee(fee.btc) } shouldBe 'right
          )
        }
      }

      "it's version is not allowed by matcher" in forAll(orderWithFeeSettingsGenerator) {
        case (order, _, orderFeeSettings) =>
          if (order.version > 1) {
            validateByMatcherSettings(orderFeeSettings, allowedOrderVersions = Set(1))(order) should produce("OrderVersionDenied")
            validateByMatcherSettings(orderFeeSettings)(order) shouldBe 'right
          } else {
            validateByMatcherSettings(orderFeeSettings, allowedOrderVersions = Set(1))(order) shouldBe 'right
            validateByMatcherSettings(orderFeeSettings)(order) shouldBe 'right
          }
      }

      "amount or price does not meet matcher's settings requirements" in {
        def decimalsGen(asset: Asset): Gen[Int] = asset.fold(Gen.const(defaultAssetDecimals))(_ => Gen.choose(0, defaultAssetDecimals))
        val sizeGen: Gen[Double]                = Gen.choose(1, 10000000).map(s => BigDecimal(s) / BigDecimal(10).pow(8)).map(_.toDouble)

        val preconditions = for {
          (order, sender, orderFeeSettings) <- orderWithFeeSettingsGenerator
          amountAssetDecimals               <- decimalsGen(order.assetPair.amountAsset)
          priceAssetDecimals                <- decimalsGen(order.assetPair.priceAsset)
          stepSize                          <- sizeGen
          tickSize                          <- sizeGen
        } yield (order, sender, orderFeeSettings, amountAssetDecimals, priceAssetDecimals, stepSize, tickSize)

        forAll(preconditions) {
          case (order, sender, orderFeeSettings, amountAssetDecimals, priceAssetDecimals, stepSize, tickSize) =>
            def normalizeAmount(value: Double): Long = Normalization.normalizeAmountAndFee(value, amountAssetDecimals) // value * 10 ^ amountAssetDecimals
            def normalizePrice(value: Double): Long  = Normalization.normalizePrice(value, amountAssetDecimals, priceAssetDecimals)

            def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals)
            def denormalizePrice(value: Long): Double  = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals)

            val normalizedStepSize = normalizeAmount(stepSize).max(2) // if normalized size == 1 then all amounts/prices are multiple of size
            val normalizedTickSize = normalizePrice(tickSize).max(2)

            def getRestrictionsByOrder(order: Order, mergeSmallPrices: Boolean = false): OrderRestrictionsSettings = OrderRestrictionsSettings(
              stepAmount = denormalizeAmount(normalizedStepSize),
              minAmount = denormalizeAmount(order.amount / 2),
              maxAmount = denormalizeAmount(order.amount * 2),
              stepPrice = denormalizePrice(normalizedTickSize),
              minPrice = denormalizePrice(order.price / 2),
              maxPrice = denormalizePrice(order.price * 2)
            )

            def updateOrderAmount(ord: Order, amt: Long): Order = Order.sign(ord.updateAmount(amt), sender)
            def updateOrderPrice(ord: Order, prc: Long): Order  = Order.sign(ord.updatePrice(prc), sender)

            val correctPrice  = (order.price / normalizedTickSize * normalizedTickSize).max(normalizedTickSize)                                     // price is a multiple of the tick size
            val correctAmount = (Order.correctAmount(order.amount, correctPrice) / normalizedStepSize * normalizedStepSize).max(normalizedStepSize) // amount is a multiple of the step size

            val defaultOrder        = Order.sign(order.updatePrice(correctPrice).updateAmount(correctAmount), sender)
            val defaultRestrictions = getRestrictionsByOrder(defaultOrder)

            val emptyRestrictionsMap            = Map.empty[AssetPair, OrderRestrictionsSettings]
            val restrictionsMapWithNonOrderPair = Map(AssetPair.createAssetPair("ETH", "BTC").get -> defaultRestrictions)
            val defaultRestrictionsMap          = Map(defaultOrder.assetPair -> defaultRestrictions)

            val orderValidator: Map[AssetPair, OrderRestrictionsSettings] => Order => Result[Order] =
              map => setScriptsAndValidate(orderFeeSettings, orderRestrictions = map)(None, None, None, None, amountAssetDecimals, priceAssetDecimals)

            orderValidator(emptyRestrictionsMap)(defaultOrder) shouldBe 'right
            orderValidator(restrictionsMapWithNonOrderPair)(defaultOrder) shouldBe 'right
            orderValidator(defaultRestrictionsMap)(defaultOrder) shouldBe 'right

            val orderWithNonMultipleAmount = updateOrderAmount(defaultOrder, defaultOrder.amount + 1)

            val restrictionsMapWithBigMinAmount =
              Map(defaultOrder.assetPair -> defaultRestrictions.copy(minAmount = denormalizeAmount((defaultOrder.amount * 1.5).toLong)))

            val restrictionsMapWithSmallMaxAmount =
              Map(defaultOrder.assetPair -> defaultRestrictions.copy(maxAmount = denormalizeAmount((defaultOrder.amount / 1.5).toLong)))

            orderValidator(defaultRestrictionsMap)(orderWithNonMultipleAmount) should produce("OrderInvalidAmount")
            orderValidator(restrictionsMapWithBigMinAmount)(defaultOrder) should produce("OrderInvalidAmount")
            orderValidator(restrictionsMapWithSmallMaxAmount)(defaultOrder) should produce("OrderInvalidAmount")

            val insignificantDecimals     = (priceAssetDecimals - amountAssetDecimals).max(0)
            val orderWithNonMultiplePrice = updateOrderPrice(defaultOrder, defaultOrder.price + BigDecimal(10).pow(insignificantDecimals).toLongExact)

            val restrictionsMapWithBigMinPrice =
              Map(defaultOrder.assetPair -> defaultRestrictions.copy(minPrice = denormalizePrice((defaultOrder.price * 1.5).toLong)))

            val restrictionsMapWithSmallMaxPrice =
              Map(defaultOrder.assetPair -> defaultRestrictions.copy(maxPrice = denormalizePrice((defaultOrder.price / 1.5).toLong)))

            orderValidator(restrictionsMapWithBigMinPrice)(defaultOrder) should produce("OrderInvalidPrice")
            orderValidator(restrictionsMapWithSmallMaxPrice)(defaultOrder) should produce("OrderInvalidPrice")

            orderValidator(defaultRestrictionsMap)(orderWithNonMultiplePrice) should produce("OrderInvalidPrice")
        }
      }

      "matcherFee is too small according to rate for matcherFeeAssetId" in forAll(orderV3WithDynamicFeeSettingsAndRateCacheGen) {
        case (order, dynamicSettings, rates) =>
          validateByMatcherSettings(dynamicSettings, rateCache = rates)(order) shouldBe 'right

          val updatedRate = rates.getRate(order.matcherFeeAssetId).map(_ + 1).get
          rates.upsertRate(order.matcherFeeAssetId, updatedRate)

          validateByMatcherSettings(dynamicSettings, rateCache = rates)(order) should produce("FeeNotEnough")
      }

      "price of market order is invalid" in forAll(orderGenerator) {
        case (order, _) =>
          def getBalance(spentAssetBalance: Long): Asset => Long = createTradableBalanceBySpentAssetWithFee(order)(spentAssetBalance)

          def validateOrder(side: (Long, Long)*)(tradableBalance: Asset => Long): Order => Result[AcceptedOrder] = {
            val levels      = side.map { case (p, a) => LevelAgg(a, p) }
            val counterSide = if (order.isBuyOrder) AggregatedSnapshot(asks = levels) else AggregatedSnapshot(bids = levels)
            validateMarketOrderByAccountStateAware(counterSide)(tradableBalance)
          }

          val orderAmount  = order.amount
          val orderPrice   = order.price
          val invalidPrice = if (order.isBuyOrder) orderPrice + 1 else orderPrice - 1
          val betterPrice  = if (order.isBuyOrder) orderPrice - 1 else orderPrice + 1
          val enough       = if (order.isBuyOrder) MatcherModel.getCost(orderAmount, orderPrice) else orderAmount

          // validate market order by market state and order's owner balance
          validateOrder { orderPrice   -> orderAmount } { getBalance(enough) }(order) shouldBe 'right
          validateOrder { orderPrice   -> orderAmount } { getBalance(enough - 1) }(order) should produce("BalanceNotEnough")
          validateOrder { invalidPrice -> orderAmount } { getBalance(enough * 2) }(order) should produce("InvalidMarketOrderPrice")

          validateOrder(Seq.empty[(Long, Long)]: _*) { getBalance(enough) }(order) shouldBe 'right

          validateOrder(
            orderPrice   -> (orderAmount - 1), // level price is OK, amount is NOT OK
            invalidPrice -> orderAmount // level price is NOT OK
          ) { getBalance(enough) }(order) should produce("InvalidMarketOrderPrice")

          validateOrder(
            betterPrice  -> (orderAmount / 2), // level price is OK, amount is NOT OK
            orderPrice   -> (orderAmount / 2 + 1), // level price is OK, amount is OK
            invalidPrice -> orderAmount // level price is NOT OK, this doesn't matter
          ) { getBalance(enough) }(order) shouldBe 'right
      }
    }

    "verify script of matcherFeeAssetId" in {
      forAll(orderV3WithFeeSettingsGenerator) {
        case (order, orderFeeSettings) =>
          def setFeeAssetScriptAndValidate(matcherFeeAssetScript: Option[Script]): Result[Order] =
            setScriptsAndValidate(orderFeeSettings)(None, None, matcherFeeAssetScript, None)(order)

          val (invalidScript, _) = ScriptCompiler.compile("(5 / 0) == 2").explicitGet()
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
      val script = ScriptCompiler("true && (height > 0)", isAssetScript = false).explicitGet()._1
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
        val script = ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
        (bc.accountScript _).when(account.toAddress).returns(Some(script)).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)).left.map(_.toJson(errorContext)) should produce("An access to the blockchain.height is denied on DEX")
      }
    }
  }

  "sunny day test when order meets matcher's settings requirements" in forAll(orderWithFeeSettingsGenerator) {
    case (order, _, orderFeeSettings) => validateByMatcherSettings(orderFeeSettings)(order) shouldBe 'right
  }

  private def portfolioTest(p: Portfolio)(f: (Order => OrderValidator.Result[Order], Blockchain) => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetScript _).when(wbtc).returns(None)
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()
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
    (bc.assetScript _).when(wbtc).returns(None)
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()

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
                                   rateCache)(_)

  private def tradableBalance(p: Portfolio)(assetId: Asset): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: Blockchain) = new ExchangeTransactionCreator(blockchain, MatcherAccount, matcherSettings)

  private def asa[A](
      p: Portfolio = defaultPortfolio,
      orderStatus: ByteStr => Boolean = _ => false,
      o: Order = newBuyOrder
  )(f: OrderValidator.Result[AcceptedOrder] => A): A =
    f(OrderValidator.accountStateAware(o.sender, tradableBalance(p), 0, orderStatus, _ => OrderBook.AggregatedSnapshot())(LimitOrder(o)))

  private def validateMarketOrderByAccountStateAware(aggregatedSnapshot: AggregatedSnapshot)(b: Asset => Long): Order => Result[AcceptedOrder] = {
    order =>
      OrderValidator.accountStateAware(
        sender = order.sender.toAddress,
        tradableBalance = b,
        activeOrderCount = 0,
        orderExists = _ => false,
        orderBookCache = _ => aggregatedSnapshot,
      ) { MarketOrder(order, b) }
  }

  private def msa(ba: Set[Address], o: Order) =
    OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, Set.empty, matcherSettings, rateCache) _

  private def createTradableBalanceBySpentAssetWithFee(order: Order)(balanceBySpentAsset: Long): Asset => Long = {
    (Map(order.getSpendAssetId -> balanceBySpentAsset) |+| Map(order.matcherFeeAssetId -> order.matcherFee)).apply _
  }

  private def validateByMatcherSettings(orderFeeSettings: OrderFeeSettings,
                                        blacklistedAssets: Set[IssuedAsset] = Set.empty[IssuedAsset],
                                        allowedAssetPairs: Set[AssetPair] = Set.empty[AssetPair],
                                        allowedOrderVersions: Set[Byte] = Set(1, 2, 3),
                                        rateCache: RateCache = rateCache): Order => Result[Order] =
    order =>
      OrderValidator
        .matcherSettingsAware(
          MatcherAccount,
          Set.empty,
          blacklistedAssets,
          matcherSettings.copy(orderFee = orderFeeSettings, allowedAssetPairs = allowedAssetPairs, allowedOrderVersions = allowedOrderVersions),
          rateCache
        )(order)

  private def setScriptsAndValidate(orderFeeSettings: OrderFeeSettings,
                                    orderRestrictions: Map[AssetPair, OrderRestrictionsSettings] = matcherSettings.orderRestrictions)(
      amountAssetScript: Option[Script],
      priceAssetScript: Option[Script],
      matcherFeeAssetScript: Option[Script],
      matcherAccountScript: Option[Script],
      amountAssetDecimals: Int = defaultAssetDecimals,
      priceAssetDecimals: Int = defaultAssetDecimals,
      matcherFeeAssetDecimals: Int = defaultAssetDecimals)(order: Order): OrderValidator.Result[Order] = {

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
      (order.assetPair.amountAsset, amountAssetScript, amountAssetDecimals),
      (order.assetPair.priceAsset, priceAssetScript, priceAssetDecimals),
      (order.matcherFeeAssetId, matcherFeeAssetScript, matcherFeeAssetDecimals)
    )

    (blockchain.accountScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript)
    (blockchain.hasScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript.isDefined)

    (blockchain.accountScript _).when(order.sender.toAddress).returns(None)
    (blockchain.hasScript _).when(order.sender.toAddress).returns(false)

    val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

    OrderValidator
      .blockchainAware(blockchain, transactionCreator, MatcherAccount.toAddress, ntpTime, orderFeeSettings, orderRestrictions, rateCache)(order)
  }

  private def createOrder(orderType: OrderType,
                          amount: Long,
                          price: Long,
                          matcherFee: Long = 0.003.waves,
                          version: Byte = 3,
                          matcherFeeAsset: Asset = Waves): Order = {
    Order(
      sender = KeyPair("seed".getBytes("utf-8")),
      matcher = MatcherAccount,
      pair = pairWavesBtc,
      orderType = orderType,
      amount = amount,
      price = price,
      timestamp = ntpNow,
      expiration = ntpNow + 10000L,
      matcherFee = matcherFee,
      version = version,
      matcherFeeAssetId = matcherFeeAsset
    )
  }

  private implicit class OrderOps(order: Order) {
    def isBuyOrder: Boolean = order.orderType match { case OrderType.BUY => true; case _ => false }
  }
}
