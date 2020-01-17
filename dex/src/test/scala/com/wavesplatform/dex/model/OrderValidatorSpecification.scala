package com.wavesplatform.dex.model

import com.google.common.base.Charsets
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.WithDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.feature.BlockchainFeatures
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1, OrderV2}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.clients.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.OrderBook.AggregatedSnapshot
import com.wavesplatform.dex.model.OrderValidator.{AsyncBlockchain, Result}
import com.wavesplatform.dex.settings.AssetType.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, FixedSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.dex.settings.{AssetType, DeviationsSettings, OrderRestrictionsSettings}
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import com.wavesplatform.dex.time.TestTime
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import mouse.any._
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OrderValidatorSpecification
    extends AnyWordSpec
    with WithDB
    with Matchers
    with MatcherSpecBase
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(btc -> 10.btc))

  private implicit val errorContext: ErrorFormatterContext = _ => defaultAssetDescription.decimals

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
        blockchainTest() { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Allowed)
          activate(bc, _ => false)
          awaitResult { ov(newBuyOrder(scripted)) } should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        blockchainTest() { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Allowed)
          activate(bc, _ => false)
          awaitResult { ov(newBuyOrder(scripted)) } should produce("AccountFeatureUnsupported")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        blockchainTest() { (ov, bc) =>
          assignScript(bc, scripted.toAddress, RunScriptResult.Denied)
          activate(bc, _ == BlockchainFeatures.SmartAccountTrading.id)
          awaitResult { ov(newBuyOrder(scripted, version = 2)) } should produce("AccountScriptDeniedOrder")
        }
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp + offset).updateSender(pk), pk)

        OrderValidator.timeAware(tt)(signed) should produce("WrongExpiration")
      }

      "amount is invalid" in {
        val pk = KeyPair(randomBytes())
        val unsigned = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(amount = 0L)
          case x: OrderV2 => x.copy(amount = 0L)
        }
        val signed = Order.sign(unsigned, pk)
        OrderValidator.timeAware(ntpTime)(signed).left.map(_.toJson) should produce("amount should be > 0")
      }

      "order signature is invalid" in blockchainTest() { (ov, bc) =>
        val pk = KeyPair(randomBytes())

        assignNoScript(bc, pk.toAddress)

        val order = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
          case x: OrderV2 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
        }

        awaitResult { ov(order) } should produce("InvalidSignature")
      }

      "order exists" in {

        val pk = KeyPair(randomBytes())
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true, _ => OrderBook.AggregatedSnapshot())(_)

        ov(LimitOrder(newBuyOrder(pk, 1000))) should produce("OrderDuplicate")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetGen(1), accountGen, Gen.choose(1, 7)) {
        case (amountAsset, sender, amountDecimals) =>
          blockchainTest(
            assetDescriptions = getDefaultAssetDescriptions(amountAsset -> BriefAssetDescription("AssetName", amountDecimals, hasScript = false))
          ) { (ov, bc) =>
            assignNoScript(bc, sender.toAddress)
            assignNoScript(bc, MatcherAccount.toAddress)
            assignNoScript(bc, amountAsset)
            activate(bc, _ == BlockchainFeatures.SmartAccountTrading.id)

            val price = BigDecimal(10).pow(-amountDecimals - 1)

            awaitResult {
              ov(
                buy(
                  AssetPair(amountAsset, Waves),
                  10.waves,
                  price,
                  matcherFee = Some(0.003.waves),
                  sender = Some(sender)
                )
              )
            } should produce("PriceLastDecimalsMustBeZero")
          }
      }

      "matcherFeeAssetId is blacklisted" in {
        val preconditions = for {
          matcherFeeAsset <- arbitraryAssetGen map (asset => IssuedAsset(asset.compatId.get))
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
            fixedFeeAsset    <- arbitraryAssetGen
            fixedFeeSettings <- fixedSettingsGenerator(fixedFeeAsset)
          } yield (order, fixedFeeSettings)

        forAll(preconditions) {
          case (order, fixedFeeSettings) => validateByMatcherSettings(fixedFeeSettings)(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (dynamic mode and incorrect asset)" in {
        forAll(orderV3WithPredefinedFeeAssetGenerator()) {
          case (_, order) => validateByMatcherSettings(DynamicSettings(order.matcherFee, false))(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId specified as WAVES Base58 string" in {

        import play.api.libs.json.Json

        val order = Json.fromJson[Order](createOrder(AssetPair(btc, usd), SELL, 100, 3.0).json() ++ Json.obj("matcherFeeAssetId" -> "WAVES")).get

        validateByMatcherSettings { DynamicSettings(0.003.waves, false) }(order).left.get.message.text should include(
          """But given "WAVES" as Base58 string. Remove this field if you want to specify WAVES in JSON"""
        )
      }

      "matcherFee is not enough (percent mode)" in {
        def validateByPercentSettings(assetType: AssetType): Order => Result[Order] = validateByMatcherSettings { PercentSettings(assetType, 0.3) }

        withClue("AMOUNT/RECEIVING asset type, min fee = 0.3%, fee should be >= 1.5.waves\n") {
          val order = createOrder(wavesBtcPair, OrderType.BUY, 500.waves, price = 0.00011162, matcherFee = 1.5.waves, feeAsset = Waves)
          Seq(AssetType.AMOUNT, AssetType.RECEIVING).foreach { assetType =>
            validateByPercentSettings(assetType) { order } shouldBe 'right
            validateByPercentSettings(assetType) { order.updateFee(1.49999999.waves) } should produce("FeeNotEnough")
          }
        }

        withClue("PRICE/SPENDING asset type, min fee = 0.3%, fee should be >= 0.00016743.btc\n") {
          val order = createOrder(wavesBtcPair, OrderType.BUY, 500.waves, price = 0.00011162, matcherFee = 0.00016743.btc, feeAsset = btc)
          Seq(AssetType.PRICE, AssetType.SPENDING).foreach { assetType =>
            validateByPercentSettings(assetType) { order } shouldBe 'right
            validateByPercentSettings(assetType) { order.updateFee(0.00016742.btc) } should produce("FeeNotEnough")
          }
        }
      }

      "matcherFee is not enough (fixed mode)" in {
        val validateByFixedSettings: Order => Result[Order] = validateByMatcherSettings { FixedSettings(usd, 0.03.usd) }
        withClue("Fee should be >= 0.03.usd\n") {
          val order = createOrder(wavesBtcPair, OrderType.BUY, 100.waves, price = 0.00011162, matcherFee = 0.03.usd, feeAsset = usd)
          validateByFixedSettings { order } shouldBe 'right
          validateByFixedSettings { order.updateFee(0.02.usd) } should produce("FeeNotEnough")
        }
      }

      "matcherFee is not enough (dynamic mode)" in {
        val validateByDynamicSettings: Order => Result[Order] = validateByMatcherSettings(DynamicSettings(0.003.waves, false), rateCache = rateCache)

        /**
          * asset rate = price of 1 Waves in that asset;
          * fee should be >= base fee * 10 pow (fee asset decimals - 8) * rate, ceiling round mode
          */
        withClue("Fee in USD (2 decimals) should be >= 0.02.usd\n") {
          val order = createOrder(wavesUsdPair, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.02.usd, feeAsset = usd)
          validateByDynamicSettings { order } shouldBe 'right
          validateByDynamicSettings { order.updateFee(0.01.usd) } should produce("FeeNotEnough")
        }

        withClue("Fee in BTC (8 decimals) should be >= 0.00000034.btc\n") {
          val order = createOrder(wavesBtcPair, OrderType.BUY, 100.waves, price = 0.00011162, matcherFee = 0.00000034.btc, feeAsset = btc)
          validateByDynamicSettings { order } shouldBe 'right
          validateByDynamicSettings { order.updateFee(0.00000033.btc) } should produce("FeeNotEnough")
        }
      }

      "matcherFee is not enough (blockchain aware)" in {

        def validateFeeByBlockchain(priceAssetScript: Option[RunScriptResult] = None,
                                    matcherScript: Option[RunScriptResult] = None): Order => Result[Order] = { order =>
          awaitResult {
            validateByBlockchain { DynamicSettings(0.003.waves, false) }(priceAssetScript = priceAssetScript,
                                                                         matcherAccountScript = matcherScript,
                                                                         matcherFeeAssetScript = priceAssetScript,
                                                                         rateCache = rateCache)(order)
          }
        }

        def updateOrder(order: Order, f: Order => Order): Order = Order.sign(f(order), senderKeyPair)

        withClue(s"Fee in USD (2 decimals, rate = 3.7) ") {

          val order  = createOrder(wavesUsdPair, OrderType.BUY, 1.waves, price = 3.7, matcherFee = 0.02.usd, feeAsset = usd)
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
          createOrder(wavesUsdPair, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.003.waves, version = version, feeAsset = Waves)
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
            awaitResult { validateByBlockchain(orderFeeSettings)(amountAssetScript, priceAssetScript, None, matcherAccountScript)(order) }

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
        val orderFeeSettings  = DynamicSettings(0.003.waves, false)

        val buyOrder  = createOrder(wavesBtcPair, OrderType.BUY, amount = 250.waves, price = 0.00011081)
        val sellOrder = createOrder(wavesBtcPair, OrderType.SELL, amount = 250.waves, price = 0.00011081)

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

        val priceValidationWithNoBounds = OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, None, None))) _

        withClue("order price can be any if bids & asks don't exist") {
          for (order <- Array(buyOrder, sellOrder)) {
            priceValidationWithNoBounds { order } shouldBe 'right
            (lowSellOrderPrices ++ midSellOrderPrices ++ highSellOrderPrices)
              .foreach(price => priceValidationWithNoBounds { order.updatePrice(price.btc) } shouldBe 'right)
          }
        }

        val priceValidationWithLowerBound =
          OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, Some(bestBid), None))) _

        withClue("order price has only lower bound if there are no asks") {
          priceValidationWithNoBounds { buyOrder } shouldBe 'right
          lowBuyOrderPrices.foreach(price => priceValidationWithLowerBound { buyOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))

          (midBuyOrderPrices ++ highBuyOrderPrices).foreach(price =>
            priceValidationWithLowerBound { buyOrder.updatePrice(price.btc) } shouldBe 'right)

          priceValidationWithNoBounds { sellOrder } shouldBe 'right
          lowSellOrderPrices.foreach(price => priceValidationWithLowerBound { sellOrder.updatePrice(price.btc) } should produce("DeviantOrderPrice"))

          (midSellOrderPrices ++ highSellOrderPrices).foreach(price =>
            priceValidationWithLowerBound { sellOrder.updatePrice(price.btc) } shouldBe 'right)
        }

        val priceValidationWithUpperBound =
          OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(MarketStatus(None, None, Some(bestAsk)))) _

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
        val priceValidation      = OrderValidator.marketAware(orderFeeSettings, deviationSettings, Some(nonEmptyMarketStatus)) _

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

        val percentSettings   = PercentSettings(AssetType.PRICE, 1)                                // matcher fee = 1% of the deal
        val deviationSettings = DeviationsSettings(enabled = true, 100, 100, maxFeeDeviation = 10) // fee deviation = 10%

        val nonEmptyMarketStatus = MarketStatus(None, Some(bestBid), Some(bestAsk))

        val feeValidation              = OrderValidator.marketAware(percentSettings, deviationSettings, Some(nonEmptyMarketStatus)) _
        val feeValidationWithoutAsks   = OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, Some(bestBid), None))) _
        val feeValidationWithoutBids   = OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, None, Some(bestAsk)))) _
        val feeValidationWithoutBounds = OrderValidator.marketAware(percentSettings, deviationSettings, Some(MarketStatus(None, None, None))) _

        // matherFee = 1% of (amount * price) = 0.000277025 => 0.00027702
        val buyOrder  = createOrder(wavesBtcPair, OrderType.BUY, 250.waves, 0.00011081, 0.00027702.btc, feeAsset = btc)
        val sellOrder = createOrder(wavesBtcPair, OrderType.SELL, 250.waves, 0.00011081, 0.00027702.btc, feeAsset = btc)

        val lowBuyOrdersFees   = Array(0, 0.00000001, 0.00001, 0.0001, 0.00012467, 0.00019999, 0.00023999, 0.00024899, 0.00024929, 0.00024934)
        val validBuyOrdersFees = Array(0.00024935, 0.00024936, 0.0002494, 0.00025001, 0.0003, 0.00123123, 1.1231231, 123123.1, 123123.12312312)

        feeValidation { buyOrder } shouldBe 'right

        withClue("buy order fee can be any if there is no asks") {
          (lowBuyOrdersFees ++ validBuyOrdersFees).foreach { fee =>
            val updatedOrder = buyOrder.updateFee(fee.btc)
            feeValidationWithoutAsks { updatedOrder } shouldBe 'right
            feeValidationWithoutBounds { updatedOrder } shouldBe 'right
          }
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
          (lowSellOrdersFees ++ validSellOrdersFees).foreach { fee =>
            val updatedOrder = sellOrder.updateFee(fee.btc)
            feeValidationWithoutBids { updatedOrder } shouldBe 'right
            feeValidationWithoutBounds { updatedOrder } shouldBe 'right
          }
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
          createOrder(wavesUsdPair, OrderType.BUY, 100.waves, price = 3, matcherFee = 0.003.waves, version = version, feeAsset = Waves)
        }

        def validate(allowedOrderVersions: Set[Byte]): Order => Result[Order] = {
          validateByMatcherSettings(DynamicSettings(0.003.waves, false), allowedOrderVersions = allowedOrderVersions)
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
                                       amountAssetDecimals = getDefaultAssetDescriptions(Waves).decimals,
                                       priceAssetDecimals = getDefaultAssetDescriptions(usd).decimals)
        }

        val validateByTickSize: Order => Result[Order] = OrderValidator.tickSizeAware { normalizePrice(3) }

        withClue(s"Tick size = 3, order price should be >= 3\n") {

          val buyOrder             = createOrder(wavesUsdPair, OrderType.BUY, amount = 1.waves, price = 3.2)
          val badPriceForBuyOrders = normalizePrice(2.99)

          validateByTickSize { buyOrder } shouldBe 'right
          validateByTickSize { buyOrder.updatePrice(badPriceForBuyOrders) } should produce("OrderInvalidPriceLevel")
          validateByTickSize { buyOrder.updateType(OrderType.SELL).updatePrice(badPriceForBuyOrders) } shouldBe 'right
        }
      }

      "amount or price does not meet matcher's settings requirements" in {
        val orderRestrictions = Map { wavesUsdPair -> OrderRestrictionsSettings(0.5, 0.5, 100, 0.5, 0.5, 100) }

        def orderWith(amount: Long, price: Double): Order = createOrder(wavesUsdPair, OrderType.BUY, amount, price)

        def validateByAmountAndPrice(orderRestrictions: Map[AssetPair, OrderRestrictionsSettings] = orderRestrictions): Order => Result[Order] =
          order => awaitResult { validateByBlockchain(DynamicSettings(0.003.waves, false), orderRestrictions)()(order) }

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

      "matcherFee is too small according to rate of fee asset" in {

        val rateCache: RateCache                   = RateCache.inMem
        val validateByRate: Order => Result[Order] = validateByMatcherSettings(DynamicSettings(0.003.waves, false), rateCache = rateCache)
        val order: Order                           = createOrder(wavesUsdPair, BUY, 1.waves, 3.00, 0.01.usd, feeAsset = usd)

        withClue("USD rate = 3.33, fee should be >= 0.01 usd\n") {
          rateCache.upsertRate(usd, 3.33)
          validateByRate(order) shouldBe 'right
        }

        withClue("USD rate = 3.34, fee should be >= 0.02 usd\n") {
          rateCache.upsertRate(usd, 3.34)
          validateByRate(order) should produce("FeeNotEnough")
        }
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
          validateByPrice { createOrder(wavesBtcPair, BUY, amount = 100.waves, price = 0.00011842) } shouldBe 'right                           // the lowest acceptable price for buying 100.waves
          validateByPrice { createOrder(wavesBtcPair, BUY, amount = 100.waves, price = 0.00011841) } should produce("InvalidMarketOrderPrice") // too low price (can only buy 30 + 36 + 23 = 89.waves)
        }

        withClue("SELL: in order to sell 100.waves price should be <= 0.00011787.btc, otherwise sell less\n") {
          validateByPrice { createOrder(wavesBtcPair, SELL, amount = 100.waves, price = 0.00011787) } shouldBe 'right                           // the highest acceptable price for selling 100.waves
          validateByPrice { createOrder(wavesBtcPair, SELL, amount = 100.waves, price = 0.00011788) } should produce("InvalidMarketOrderPrice") // too high price (can only sell 12 + 85 = 97.waves)
        }

        withClue("BUY: fee in received asset, required balance: Waves -> 0, BTC -> 0.01182201\n") {
          val marketOrder = createOrder(wavesBtcPair, BUY, buyAmount, buyPrice, feeAsset = Waves, matcherFee = 0.003.waves)
          validateByTradableBalance { Map(btc -> 0.01182201.btc) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182200.btc) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("BUY: fee in spent asset, required balance: Waves -> 0, BTC -> 0.01182201 + 0.00000035 = 0.01182236\n") {
          val marketOrder = createOrder(wavesBtcPair, BUY, buyAmount, buyPrice, feeAsset = btc, matcherFee = 0.00000035.btc)
          validateByTradableBalance { Map(btc -> 0.01182236.btc) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182235.btc) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("BUY: fee in third asset, required balance: Waves -> 0, BTC -> 0.01182201, ETH = 0.00649308\n") {
          val marketOrder = createOrder(wavesBtcPair, BUY, buyAmount, buyPrice, feeAsset = eth, matcherFee = 0.00649308.eth)
          validateByTradableBalance { Map(btc -> 0.01182201.btc, eth -> 0.00649308.eth) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(btc -> 0.01182201.btc, eth -> 0.00649307.eth) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in received asset, required balance: Waves -> 100, BTC -> 0\n") {
          val marketOrder = createOrder(wavesBtcPair, SELL, sellAmount, sellPrice, feeAsset = btc, matcherFee = 0.00000035.btc)
          validateByTradableBalance { Map(Waves -> 100.00000000.waves) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 99.99999999.waves) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in spent asset, required balance: Waves -> 100.003, BTC -> 0\n") {
          val marketOrder = createOrder(wavesBtcPair, SELL, sellAmount, sellPrice, feeAsset = Waves, matcherFee = 0.003.waves)
          validateByTradableBalance { Map(Waves -> 100.00300000.waves) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 100.00299999.waves) }(marketOrder) should produce("BalanceNotEnough")
        }

        withClue("SELL: fee in third asset, required balance: Waves -> 100, BTC -> 0, ETH -> 0.00649308\n") {
          val marketOrder = createOrder(wavesBtcPair, SELL, sellAmount, sellPrice, feeAsset = eth, matcherFee = 0.00649308.eth)
          validateByTradableBalance { Map(Waves -> 100.waves, eth -> 0.00649308.eth) }(marketOrder) shouldBe 'right
          validateByTradableBalance { Map(Waves -> 100.waves, eth -> 0.00649307.eth) }(marketOrder) should produce("BalanceNotEnough")
        }
      }

      "matcherFee is not enough (zero-maker-double-tacker = true requires double fee)" in {

        val rateCache = RateCache.inMem unsafeTap { _.upsertRate(btc, 0.00011167) }

        def orderWithFee(fee: Long, feeAsset: Asset = Waves): Order =
          createOrder(wavesUsdPair, BUY, 1.waves, 3.0, matcherFee = fee, feeAsset = feeAsset)

        def validateByFee(doubleTaker: Boolean)(order: Order): Result[Order] =
          validateByMatcherSettings(DynamicSettings(0.003.waves, doubleTaker), rateCache = rateCache)(order)

        def validateByFeeWithMatcherScript(doubleTaker: Boolean)(order: Order): Result[Order] =
          awaitResult {
            validateByBlockchain(DynamicSettings(0.003.waves, doubleTaker))(
              matcherAccountScript = Some(RunScriptResult.Allowed),
              rateCache = rateCache
            )(order)
          }

        validateByFee(doubleTaker = false) { orderWithFee(0.003.waves) } shouldBe 'right
        validateByFee(doubleTaker = true) { orderWithFee(0.006.waves) } shouldBe 'right
        validateByFee(doubleTaker = true) { orderWithFee(0.00599999.waves) } should produce("FeeNotEnough")

        withClue("0.003.waves = 0.00000034.btc with BTC rate = 0.00011167\n") {
          validateByFee(doubleTaker = false) { orderWithFee(0.00000034.btc, btc) } shouldBe 'right
          validateByFee(doubleTaker = true) { orderWithFee(0.00000068.btc, btc) } shouldBe 'right
          validateByFee(doubleTaker = true) { orderWithFee(0.00000067.btc, btc) } should produce("FeeNotEnough")
        }

        val extraWavesFee = 0.004.waves

        validateByFeeWithMatcherScript(doubleTaker = false) { orderWithFee(0.003.waves + extraWavesFee) } shouldBe 'right
        validateByFeeWithMatcherScript(doubleTaker = true) { orderWithFee(0.006.waves + extraWavesFee) } shouldBe 'right
        validateByFeeWithMatcherScript(doubleTaker = true) { orderWithFee(0.00599999.waves + extraWavesFee) } should produce("FeeNotEnough")

        withClue("0.004.waves = 0.00000045.btc with BTC rate = 0.00011167, 2 * 0.003.waves + 0.004.waves = 0.00000112.btc\n") {
          validateByFeeWithMatcherScript(doubleTaker = false) { orderWithFee(0.00000079.btc, btc) } shouldBe 'right
          validateByFeeWithMatcherScript(doubleTaker = true) { orderWithFee(0.00000112.btc, btc) } shouldBe 'right
          validateByFeeWithMatcherScript(doubleTaker = true) { orderWithFee(0.00000111.btc, btc) } should produce("FeeNotEnough")
        }
      }
    }

    "verify script of feeAsset" in {
      forAll(orderV3WithFeeSettingsGenerator) {
        case (order, orderFeeSettings) =>
          def setFeeAssetScriptAndValidate(matcherFeeAssetScript: Option[RunScriptResult]): Result[Order] =
            awaitResult { validateByBlockchain(orderFeeSettings)(None, None, matcherFeeAssetScript, None)(order) }

          if (order.feeAsset != Waves) {
            setFeeAssetScriptAndValidate(Some(RunScriptResult.ScriptError("Some error"))) should produce("AssetScriptReturnedError")
            setFeeAssetScriptAndValidate(Some(RunScriptResult.Denied)) should produce("AssetScriptDeniedOrder")
            setFeeAssetScriptAndValidate(None) shouldBe 'right
          }
      }
    }

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }

// @TODO run this to runScript
//
//    "meaningful error for undefined functions in matcher" in blockchainTest() { (ov, bc) =>
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
//      val portfolio = Portfolio(10.waves,
//                                LeaseBalance.empty,
//                                Map(
//                                  asset1 -> 10.waves,
//                                  asset2 -> 10.waves
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
//        s"v$version" in blockchainTest(portfolio) { (ov, bc) =>
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
//            amount = 100.waves,
//            price = 0.0022,
//            sender = Some(pk),
//            matcherFee = Some(0.003.waves),
//            ts = Some(System.currentTimeMillis()),
//            version = version.toByte
//          )
//          assignNoScript(bc, o.sender.toAddress)
//          f(ov, bc, o)
//        }
//      }
//    }

    "deny OrderV2 if SmartAccountTrading hasn't been activated yet" in forAll(accountGen) { account =>
      blockchainTest() { (ov, bc) =>
        activate(bc, _ => false)
        assignScript(bc, account.toAddress, RunScriptResult.Allowed)

        ov(newBuyOrder(account, version = 2)).value.foreach { _ should produce("OrderVersionUnsupported") }
      }
    }

// @TODO run this to runScript
//
//    "deny blockchain functions in account script" in forAll(accountGen) { account =>
//      blockchainTest() { (ov, bc) =>
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

  private def blockchainTest(assetDescriptions: Asset => BriefAssetDescription = getDefaultAssetDescriptions,
                             hasMatcherAccountScript: Boolean = false)(f: (Order => FutureResult[Order], AsyncBlockchain) => Any): Unit = {

    val bc = stub[AsyncBlockchain]
    val tc = exchangeTransactionCreator(bc, hasMatcherAccountScript, assetDescriptions(_).hasScript)
    val ov = mkOrderValidator(bc, tc, assetDescriptions)

    f(ov, bc)
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = {

    val bc = stub[AsyncBlockchain]
    val pk = KeyPair(randomBytes())

    activate(bc, _ == BlockchainFeatures.SmartAccountTrading.id)
    assignScript(bc, pk.toAddress, RunScriptResult.Allowed)
    assignNoScript(bc, MatcherAccount.toAddress)
    assignNoScript(bc, btc)
    assignAssetDescription(bc, btc -> mkAssetDescription(8))

    val order =
      OrderV2(
        senderPublicKey = pk,
        matcherPublicKey = MatcherAccount,
        assetPair = wavesBtcPair,
        amount = 100.waves,
        price = (0.0022 * Order.PriceConstant).toLong,
        timestamp = System.currentTimeMillis(),
        expiration = System.currentTimeMillis() + 60 * 60 * 1000L,
        matcherFee = 0.003.waves,
        orderType = OrderType.BUY,
        proofs = proofs
      )

    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)

    awaitResult { ov(order) } shouldBe 'right
  }

  private def mkAssetDescription(decimals: Int): BriefAssetDescription = BriefAssetDescription(name = "name", decimals = decimals, hasScript = false)

  private def newBuyOrder: Order =
    buy(pair = wavesBtcPair, amount = 100.waves, price = 0.0022, matcherFee = Some(0.003.waves))

  private def newBuyOrder(pk: KeyPair, ts: Long = System.currentTimeMillis, version: Byte = 1) =
    buy(
      pair = wavesBtcPair,
      amount = 100.waves,
      price = 0.0022,
      sender = Some(pk),
      matcherFee = Some(0.003.waves),
      ts = Some(ts),
      version = version
    )

  private def activate(bc: AsyncBlockchain, isActive: Function[Short, Boolean]): Unit = {
    (bc.isFeatureActivated _).when(*).onCall(isActive andThen Future.successful)
  }

  private def mkOrderValidator(bc: AsyncBlockchain,
                               tc: ExchangeTransactionCreator,
                               assetDescriptions: Asset => BriefAssetDescription = getDefaultAssetDescriptions): Order => FutureResult[Order] = {
    order =>
      OrderValidator.blockchainAware(
        bc,
        tc.createTransaction,
        MatcherAccount,
        ntpTime,
        matcherSettings.orderFee,
        matcherSettings.orderRestrictions,
        assetDescriptions,
        rateCache,
        hasMatcherAccountScript = false
      )(order)
  }

  private def tradableBalance(p: Portfolio)(assetId: Asset): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: AsyncBlockchain,
                                         hasMatcherAccountScript: Boolean = false,
                                         hasAssetScript: Asset => Boolean = getDefaultAssetDescriptions(_).hasScript) = {
    new ExchangeTransactionCreator(MatcherAccount, matcherSettings, hasMatcherAccountScript, hasAssetScript)
  }

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
        orderBookCache = _ => aggregatedSnapshot
      ) { MarketOrder(order, b.apply _) }
  }

  private def msa(ba: Set[Address], o: Order): Order => Result[Order] = {
    OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, matcherSettings, getDefaultAssetDescriptions(_).decimals, rateCache)
  }

  private def validateByMatcherSettings(orderFeeSettings: OrderFeeSettings,
                                        blacklistedAssets: Set[IssuedAsset] = Set.empty[IssuedAsset],
                                        allowedAssetPairs: Set[AssetPair] = Set.empty[AssetPair],
                                        allowedOrderVersions: Set[Byte] = Set(1, 2, 3),
                                        assetDecimals: Asset => Int = getDefaultAssetDescriptions(_).decimals,
                                        rateCache: RateCache = rateCache): Order => Result[Order] = { order =>
    OrderValidator
      .matcherSettingsAware(
        MatcherAccount,
        Set.empty,
        matcherSettings.copy(orderFee = orderFeeSettings,
                             allowedAssetPairs = allowedAssetPairs,
                             allowedOrderVersions = allowedOrderVersions,
                             blacklistedAssets = blacklistedAssets),
        assetDecimals,
        rateCache
      )(order)
  }

  private def validateByBlockchain(orderFeeSettings: OrderFeeSettings,
                                   orderRestrictions: Map[AssetPair, OrderRestrictionsSettings] = matcherSettings.orderRestrictions)(
      amountAssetScript: Option[RunScriptResult] = None,
      priceAssetScript: Option[RunScriptResult] = None,
      matcherFeeAssetScript: Option[RunScriptResult] = None,
      matcherAccountScript: Option[RunScriptResult] = None,
      assetDescriptions: Asset => BriefAssetDescription = getDefaultAssetDescriptions,
      rateCache: RateCache = rateCache)(order: Order): FutureResult[Order] = {

    val blockchain = stub[AsyncBlockchain]

    activate(
      blockchain,
      List(
        BlockchainFeatures.SmartAccountTrading,
        BlockchainFeatures.OrderV3,
        BlockchainFeatures.SmartAssets
      ).map(_.id).contains
    )

    def prepareAssets(assetsAndScripts: (Asset, Option[RunScriptResult], Int)*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption, decimals) =>
        assignAssetDescription(blockchain, asset -> mkAssetDescription(decimals))
        assignScript(blockchain, asset, scriptOption)
      case _ =>
    }

    prepareAssets(
      (order.assetPair.amountAsset, amountAssetScript, assetDescriptions(order.assetPair.amountAsset).decimals),
      (order.assetPair.priceAsset, priceAssetScript, assetDescriptions(order.assetPair.priceAsset).decimals),
      (order.feeAsset, matcherFeeAssetScript, assetDescriptions(order.feeAsset).decimals)
    )

    assignScript(blockchain, MatcherAccount.toAddress, matcherAccountScript)
    assignNoScript(blockchain, order.sender.toAddress)

    val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

    val assetsDescriptions = {
      Set(order.assetPair.amountAsset -> amountAssetScript, order.assetPair.priceAsset -> priceAssetScript, order.feeAsset -> matcherFeeAssetScript)
        .foldLeft(defaultAssetDescriptionsMap) {
          case (descMap, (asset, maybeScript)) => descMap.updated(asset, descMap(asset).copy(hasScript = maybeScript.nonEmpty))
        }
    }

    OrderValidator
      .blockchainAware(
        blockchain,
        transactionCreator,
        MatcherAccount.toAddress,
        ntpTime,
        orderFeeSettings,
        orderRestrictions,
        assetDescriptions = assetsDescriptions,
        rateCache,
        matcherAccountScript.nonEmpty
      )(order)
  }

  private def assignScript(bc: AsyncBlockchain, address: Address, result: RunScriptResult): Unit = {
    (bc.hasScript(_: Address)).when(address).returns { Future.successful(true) }
    (bc.runScript(_: Address, _: Order)).when(address, *).onCall((_, _) => Future.successful(result))
  }

  private def assignScript(bc: AsyncBlockchain, address: Address, result: Option[RunScriptResult]): Unit = result match {
    case None => (bc.hasScript(_: Address)).when(address).returns { Future.successful(false) }
    case Some(r) =>
      (bc.hasScript(_: Address)).when(address).returns { Future.successful(true) }
      (bc.runScript(_: Address, _: Order)).when(address, *).onCall((_, _) => Future.successful(r))
  }

  private def assignScript(bc: AsyncBlockchain, asset: IssuedAsset, result: Option[RunScriptResult]): Unit = result match {
    case None => (bc.hasScript(_: IssuedAsset)).when(asset).returns { Future.successful(false) }
    case Some(r) =>
      (bc.hasScript(_: IssuedAsset)).when(asset).returns { Future.successful(true) }
      (bc.runScript(_: IssuedAsset, _: ExchangeTransaction)).when(asset, *).onCall((_, _) => Future.successful(r))
  }

  private def assignNoScript(bc: AsyncBlockchain, address: Address): Unit =
    (bc.hasScript(_: Address)).when(address).returns(Future.successful(false))

  private def assignNoScript(bc: AsyncBlockchain, asset: IssuedAsset): Unit =
    (bc.hasScript(_: IssuedAsset)).when(asset).returns(Future.successful(false))

  private def assignAssetDescription(bc: AsyncBlockchain, xs: (IssuedAsset, BriefAssetDescription)*): Unit =
    xs.foreach {
      case (asset, desc) => (bc.assetDescription _).when(asset).onCall((_: IssuedAsset) => Future.successful { Some(desc) })
    }
}
