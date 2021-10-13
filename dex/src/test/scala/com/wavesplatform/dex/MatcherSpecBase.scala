package com.wavesplatform.dex

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.ws.protocol.WsError
import com.wavesplatform.dex.api.ws.entities.WsMatchTransactionInfo
import com.wavesplatform.dex.asset.DoubleOps
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.TestRateDb
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.model.{Normalization, Price}
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV3}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransactionResult, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.domain.{crypto => wcrypto}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.Events.{OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{BuyLimitOrder, LimitOrder, OrderValidator, SellLimitOrder, _}
import com.wavesplatform.dex.queue.ValidatedCommand.{CancelOrder, DeleteOrderBook, PlaceMarketOrder, PlaceOrder}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.settings.OrderFeeSettings._
import com.wavesplatform.dex.settings.{loadConfig, AssetType, MatcherSettings, OrderFeeSettings}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.time.SystemTime
import com.wavesplatform.dex.waves.WavesFeeConstants
import io.qameta.allure.scalatest.AllureScalatestContext
import kamon.context.Context
import mouse.any._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite
import org.scalatest.concurrent.ScalaFutures
import pureconfig.ConfigSource

import java.math.BigInteger
import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

trait MatcherSpecBase
    extends SystemTime
    with DiffMatcherWithImplicits
    with DoubleOps
    with WavesFeeConstants
    with AllureScalatestContext
    with ScalaFutures {
  _: Suite =>

  implicit override def patienceConfig: PatienceConfig = PatienceConfig(2.seconds, 5.millis)

  implicit protected val wsMatchTransactionInfoDiff: Derived[Diff[WsMatchTransactionInfo]] = Derived(
    Diff.gen[WsMatchTransactionInfo].value
      .ignore[WsMatchTransactionInfo, ByteStr](_.txId)
      .ignore[WsMatchTransactionInfo, Long](_.timestamp)
  )

  implicit protected def wsErrorDiff: Derived[Diff[WsError]] = Derived(Diff.gen[WsError].ignore[WsError, Long](_.timestamp))

  implicit protected def placeOrderDiff: Derived[Diff[PlaceOrder]] =
    Derived(Diff.gen[PlaceOrder].ignore[PlaceOrder, Option[Context]](_.maybeCtx))

  implicit protected def placeMarketOrderDiff: Derived[Diff[PlaceMarketOrder]] =
    Derived(Diff.gen[PlaceMarketOrder].ignore[PlaceMarketOrder, Option[Context]](_.maybeCtx))

  implicit protected def cancelOrderDiff: Derived[Diff[CancelOrder]] =
    Derived(Diff.gen[CancelOrder].ignore[CancelOrder, Option[Context]](_.maybeCtx))

  implicit protected def deleteOrderBookDiff: Derived[Diff[DeleteOrderBook]] =
    Derived(Diff.gen[DeleteOrderBook].ignore[DeleteOrderBook, Option[Context]](_.maybeCtx))

  implicit protected def orderCanceledDiff: Derived[Diff[OrderCanceled]] =
    Derived(Diff.gen[OrderCanceled].value.ignore[OrderCanceled, Long](_.timestamp))

  private val WalletSeed: ByteStr = ByteStr("Matcher".getBytes("utf-8"))
  private val MatcherSeed: Array[Byte] = wcrypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))

  protected val MatcherAccount: KeyPair = KeyPair(MatcherSeed)
  protected val senderKeyPair: KeyPair = mkKeyPair("seed")

  protected val btc: IssuedAsset = mkAssetId("WBTC")
  protected val usd: IssuedAsset = mkAssetId("WUSD")
  protected val eth: IssuedAsset = mkAssetId("WETH")

  protected val wavesBtcPair: AssetPair = AssetPair(Waves, btc)
  protected val wavesUsdPair: AssetPair = AssetPair(Waves, usd)
  protected val btcUsdPair: AssetPair = AssetPair(btc, usd)

  protected val rateCache: RateCache = RateCache(TestRateDb()).futureValue
    .unsafeTap(_.upsertRate(usd, 3.7))
    .unsafeTap(_.upsertRate(btc, 0.00011167))

  protected val smallFee: Option[Price] = Some(toNormalized(1))

  protected val defaultAssetDescription: BriefAssetDescription = BriefAssetDescription("Asset", 8, hasScript = false, isNft = false)

  protected val defaultAssetDescriptionsMap: Map[Asset, BriefAssetDescription] =
    Map[Asset, BriefAssetDescription](
      usd -> BriefAssetDescription("USD", 2, hasScript = false, isNft = false),
      btc -> BriefAssetDescription("BTC", 8, hasScript = false, isNft = false)
    ).withDefaultValue(defaultAssetDescription)

  protected val getDefaultAssetDescriptions: Asset => BriefAssetDescription = defaultAssetDescriptionsMap.apply

  protected def getDefaultAssetDescriptions(assetAndDesc: (Asset, BriefAssetDescription)*): Asset => BriefAssetDescription =
    defaultAssetDescriptionsMap ++ Map(assetAndDesc: _*)

  private val seqNr: AtomicLong = new AtomicLong(-1)

  protected def toNormalized(value: Long): Long = value * Order.PriceConstant

  protected def wrapCommand(n: Long, command: ValidatedCommand): ValidatedCommandWithMeta =
    ValidatedCommandWithMeta(n, System.currentTimeMillis(), command)

  protected def wrapCommand(command: ValidatedCommand): ValidatedCommandWithMeta =
    ValidatedCommandWithMeta(seqNr.incrementAndGet(), System.currentTimeMillis(), command)

  protected def wrapLimitOrder(x: Order): ValidatedCommandWithMeta = wrapLimitOrder(seqNr.incrementAndGet(), x)

  protected def wrapLimitOrder(n: Long, x: Order): ValidatedCommandWithMeta =
    wrapCommand(n, ValidatedCommand.PlaceOrder(LimitOrder(x)))

  protected def wrapMarketOrder(mo: MarketOrder): ValidatedCommandWithMeta =
    wrapCommand(ValidatedCommand.PlaceMarketOrder(mo))

  protected def getSpentAmountWithFee(order: Order): Long = {
    val lo = LimitOrder(order)
    lo.spentAmount + (if (order.getSpendAssetId == order.feeAsset) lo.fee else 0)
  }

  protected def mkKeyPair(seed: String): KeyPair = KeyPair(seed getBytes "utf-8")

  protected def createOrder(
    pair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Double,
    matcherFee: Long = matcherFee,
    version: Byte = 3,
    feeAsset: Asset = Waves,
    sender: KeyPair = senderKeyPair
  ): Order =
    Order(
      sender = sender,
      matcher = MatcherAccount,
      pair = pair,
      orderType = orderType,
      amount = amount,
      price = Normalization
        .normalizePrice(price, getDefaultAssetDescriptions(pair.amountAsset).decimals, getDefaultAssetDescriptions(pair.priceAsset).decimals),
      timestamp = ntpNow,
      expiration = ntpNow + (1000 * 60 * 60 * 24),
      matcherFee = matcherFee,
      version = version,
      feeAsset = feeAsset
    )

  protected def arbitraryAssetGen: Gen[Asset] = Gen.frequency((9, arbitraryIssuedAssetGen), (1, Gen.const(Waves)))

  protected def issuedAssetGen(prefix: Byte): Gen[IssuedAsset] =
    Gen
      .listOfN(Asset.AssetIdLength - 1, Arbitrary.arbitrary[Byte])
      .map(xs => IssuedAsset(ByteStr(Array(prefix, xs: _*))))

  protected def arbitraryIssuedAssetGen: Gen[IssuedAsset] = issuedAssetGen(Random.nextInt(Byte.MaxValue).toByte)

  protected val distinctPairGen: Gen[AssetPair] = for {
    a1 <- issuedAssetGen(1.toByte)
    a2 <- issuedAssetGen(2.toByte)
  } yield AssetPair(a1, a2)

  protected def mkAssetId(prefix: String): IssuedAsset = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    IssuedAsset(ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32)))
  }

  protected val assetPairGen: Gen[AssetPair] =
    Gen.frequency((18, distinctPairGen), (1, issuedAssetGen(1).map(AssetPair(_, Waves))), (1, issuedAssetGen(2).map(AssetPair(Waves, _))))

  private val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + System.currentTimeMillis())
  private val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(System.currentTimeMillis() - _)

  protected val matcherSettings: MatcherSettings = ConfigSource.fromConfig(loadConfig(None)).at("waves.dex").loadOrThrow[MatcherSettings]

  protected def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  private def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) value = gen.sample
    value.get
  }

  protected val maxWavesAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  private def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  private val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  protected val accountGen: Gen[KeyPair] = bytes32gen.map(arr => KeyPair(arr))

  private def buyGenerator(
    pair: AssetPair,
    amount: Long,
    price: Long,
    sender: Option[KeyPair],
    matcherFee: Option[Long],
    version: Byte,
    timestamp: Option[Long],
    feeAsset: Asset
  ): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version, feeAsset), sender)

  private def sellGenerator(
    pair: AssetPair,
    amount: Price,
    price: Price,
    sender: Option[KeyPair],
    matcherFee: Option[Price],
    timestamp: Option[Price],
    version: Byte,
    feeAsset: Asset
  ): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version, feeAsset), sender)

  protected def buy(
    pair: AssetPair,
    amount: Price,
    price: BigDecimal,
    sender: Option[KeyPair] = None,
    matcherFee: Option[Long] = None,
    ts: Option[Long] = None,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order =
    rawBuy(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version, feeAsset)

  protected def rawBuy(
    pair: AssetPair,
    amount: Long,
    price: Price,
    sender: Option[KeyPair] = None,
    matcherFee: Option[Long] = None,
    ts: Option[Long] = None,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order =
    valueFromGen(buyGenerator(pair, amount, price, sender, matcherFee, version, ts, feeAsset))._1

  protected def sell(
    pair: AssetPair,
    amount: Long,
    price: BigDecimal,
    sender: Option[KeyPair] = None,
    matcherFee: Option[Long] = None,
    ts: Option[Long] = None,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order =
    rawSell(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version, feeAsset)

  protected def rawSell(
    pair: AssetPair,
    amount: Long,
    price: Price,
    sender: Option[KeyPair] = None,
    matcherFee: Option[Long] = None,
    ts: Option[Long] = None,
    version: Byte = 1,
    feeAsset: Asset = Waves
  ): Order =
    valueFromGen(sellGenerator(pair, amount, price, sender, matcherFee, ts, version, feeAsset))._1

  protected val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  protected def orderGenerator(sender: KeyPair, pair: AssetPair): Gen[Order] =
    for {
      orderType <- orderTypeGenerator
      amount: Long <- maxWavesAmountGen
      price: Long <- Gen.choose((BigDecimal(10).pow(8) / amount).toLong.max(1), (Long.MaxValue / amount) - 100)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset <- arbitraryIssuedAssetGen
      feeAsset <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield
      if (orderVersion == 3)
        Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion, feeAsset)
      else Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion)

  protected val orderGenerator: Gen[(Order, KeyPair)] = for {
    sender: KeyPair <- accountGen
    pair <- assetPairGen
    order <- orderGenerator(sender, pair)
  } yield order -> sender

  protected val buyLimitOrderGenerator: Gen[BuyLimitOrder] =
    for {
      sender: KeyPair <- accountGen
      pair <- assetPairGen
      amount: Long <- maxWavesAmountGen
      price: Long <- maxWavesAmountGen
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
    } yield BuyLimitOrder(
      amount,
      matcherFee,
      Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion),
      BigInteger.ZERO
    )

  protected val sellLimitOrderGenerator: Gen[SellLimitOrder] =
    for {
      sender: KeyPair <- accountGen
      pair <- assetPairGen
      amount: Long <- maxWavesAmountGen
      price: Long <- maxWavesAmountGen
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
    } yield SellLimitOrder(
      amount,
      matcherFee,
      Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion),
      BigInteger.ZERO
    )

  protected val limitOrderGenerator: Gen[LimitOrder] = Gen.oneOf(sellLimitOrderGenerator, buyLimitOrderGenerator)

  protected val orderV3Generator: Gen[Order] =
    for {
      sender: KeyPair <- accountGen
      pair <- assetPairGen
      orderType <- orderTypeGenerator
      amount: Long <- maxWavesAmountGen
      price: Long <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      arbitraryAsset <- arbitraryIssuedAssetGen
      feeAsset <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield OrderV3(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, feeAsset)

  protected def orderV3WithPredefinedFeeAssetGenerator(feeAsset: Option[Asset] = None): Gen[(KeyPair, Order)] =
    for {
      sender: KeyPair <- accountGen
      pair <- distinctPairGen
      orderType <- orderTypeGenerator
      amount: Long <- maxWavesAmountGen
      price: Long <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      arbitraryAsset <- arbitraryIssuedAssetGen
    } yield sender -> OrderV3(
      sender,
      MatcherAccount,
      pair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      feeAsset getOrElse arbitraryAsset
    )

  protected val orderV3MirrorPairGenerator: Gen[((KeyPair, Order), (KeyPair, Order))] =
    for {
      senderBuy <- accountGen
      senderSell <- accountGen
      pair <- assetPairGen
      amount <- maxWavesAmountGen
      price <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestampBuy <- createdTimeGen
      timestampSell <- createdTimeGen
      expirationBuy <- maxTimeGen
      expirationSell <- maxTimeGen
      matcherFeeBuy <- maxWavesAmountGen
      matcherFeeSell <- maxWavesAmountGen
      arbitraryAsset <- arbitraryIssuedAssetGen
      feeAsset <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield (
      senderBuy -> OrderV3(
        senderBuy,
        MatcherAccount,
        pair,
        OrderType.BUY,
        Order.correctAmount(amount, price),
        price,
        timestampBuy,
        expirationBuy,
        matcherFeeBuy,
        feeAsset
      ),
      senderSell -> OrderV3(
        senderSell,
        MatcherAccount,
        pair,
        OrderType.SELL,
        Order.correctAmount(amount, price),
        price,
        timestampSell,
        expirationSell,
        matcherFeeSell,
        feeAsset
      )
    )

  protected val percentSettingsGenerator: Gen[PercentSettings] =
    for {
      assetType <- Gen.oneOf(AssetType.values.toSeq)
      minFee <- Gen.choose(0.01, 100.0)
    } yield PercentSettings(assetType, minFee)

  protected def fixedSettingsGenerator(defaultAsset: Asset, lowerMinFeeBound: Long = 1, upperMinFeeBound: Long = 1000000L): Gen[FixedSettings] =
    for { minFee <- Gen.choose(lowerMinFeeBound, upperMinFeeBound) } yield FixedSettings(defaultAsset, minFee)

  protected def dynamicSettingsGenerator(lowerBaseFeeBound: Long = 1, upperBaseFeeBound: Long = 1000000L): Gen[DynamicSettings] =
    for { baseFee <- Gen.choose(lowerBaseFeeBound, upperBaseFeeBound) } yield DynamicSettings(baseFee, baseFee)

  private def orderFeeSettingsGenerator(defaultAssetForFixedSettings: Option[Asset] = None): Gen[OrderFeeSettings] =
    for {
      defaultAsset <- defaultAssetForFixedSettings.fold(arbitraryIssuedAssetGen)(_.fold(arbitraryIssuedAssetGen)(Gen.const))
      orderFeeSettings <- Gen.oneOf(dynamicSettingsGenerator(), fixedSettingsGenerator(defaultAsset), percentSettingsGenerator)
    } yield orderFeeSettings

  protected val orderWithoutWavesInPairAndWithFeeSettingsGenerator: Gen[(Order, KeyPair, OrderFeeSettings)] = {
    for {
      sender: KeyPair <- accountGen
      amountAsset <- arbitraryIssuedAssetGen
      priceAsset <- arbitraryIssuedAssetGen
      orderFeeSettings <- orderFeeSettingsGenerator()
      order <- orderGenerator(sender, AssetPair(amountAsset, priceAsset))
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  protected val orderV3WithFeeSettingsGenerator: Gen[(Order, OrderFeeSettings)] = {
    for {
      (sender, order) <- orderV3WithPredefinedFeeAssetGenerator()
      orderFeeSettings <- orderFeeSettingsGenerator(Some(order.feeAsset))
    } yield correctOrderByFeeSettings(order, sender, orderFeeSettings) -> orderFeeSettings
  }

  private def correctOrderByFeeSettings(
    order: Order,
    sender: KeyPair,
    orderFeeSettings: OrderFeeSettings,
    matcherFeeAssetForDynamicSettings: Option[Asset] = None,
    rateForDynamicSettings: Option[Double] = None
  ): Order = {

    val correctedOrder = (order.version, orderFeeSettings) match {
      case (3, FixedSettings(defaultAssetId, minFee)) =>
        order
          .updateFeeAsset(defaultAssetId)
          .updateFee(minFee)
      case (3, percentSettings: PercentSettings) =>
        order
          .updateFeeAsset(OrderValidator.getValidFeeAssetForSettings(order, percentSettings, rateCache).head)
          .updateFee {
            OrderValidator.getMinValidFeeForSettings(
              order,
              percentSettings,
              getDefaultAssetDescriptions(order.feeAsset).decimals,
              rateCache
            ).explicitGet()
          }
      case (_, ds @ DynamicSettings(_, _, _)) =>
        order
          .updateFeeAsset(matcherFeeAssetForDynamicSettings getOrElse Waves)
          .updateFee(
            rateForDynamicSettings.fold(ds.maxBaseFee) { rate =>
              OrderValidator.multiplyFeeByBigDecimal(ds.maxBaseFee, BigDecimal.valueOf(rate))
            }
          )
      case _ => order
    }

    Order.sign(correctedOrder, sender)
  }

  protected def mkOrderExecutedRaw(submitted: Order, counter: Order): OrderExecuted =
    mkOrderExecuted(LimitOrder(submitted), LimitOrder(counter), submitted.timestamp)

  protected def mkOrderExecutedRaw(submitted: Order, counter: Order, timestamp: Long): OrderExecuted =
    mkOrderExecuted(LimitOrder(submitted), LimitOrder(counter), timestamp)

  protected def mkOrderExecuted(submittedAo: AcceptedOrder, counterLo: LimitOrder): OrderExecuted =
    mkOrderExecuted(submittedAo, counterLo, submittedAo.order.timestamp)

  protected def mkOrderExecuted(submittedAo: AcceptedOrder, counterLo: LimitOrder, timestamp: Long): OrderExecuted = {
    val (counterExecutedFee, submittedExecutedFee) = makerTakerPartialFee(submittedAo, counterLo)
    OrderExecuted(submittedAo, counterLo, timestamp, counterExecutedFee, submittedExecutedFee, 0L)
  }

  protected def makerTakerPartialFee(submittedAo: AcceptedOrder, counterLo: LimitOrder): (Long, Long) = {
    val executedAmount = AcceptedOrder.executedAmount(submittedAo, counterLo)
    (
      AcceptedOrder.partialFee(counterLo.matcherFee, counterLo.order.amount, executedAmount),
      AcceptedOrder.partialFee(submittedAo.matcherFee, submittedAo.order.amount, executedAmount)
    )
  }

  protected def addr(seed: String): Address = privateKey(seed).toAddress
  protected def privateKey(seed: String): KeyPair = KeyPair(seed.getBytes("utf-8"))
  protected def nowTs: Long = System.currentTimeMillis()

  protected def mkExchangeTx(oe: OrderExecuted): ExchangeTransactionResult[ExchangeTransactionV2] = {
    val (sellOrder, buyOrder) = if (oe.counter.isSellOrder) (oe.counter, oe.submitted) else (oe.submitted, oe.counter)
    mkExchangeTx(buyOrder.order, sellOrder.order)
  }

  protected def mkExchangeTx(buyOrder: Order, sellOrder: Order): ExchangeTransactionResult[ExchangeTransactionV2] = ExchangeTransactionV2
    .create(
      buyOrder = buyOrder,
      sellOrder = sellOrder,
      amount = sellOrder.amount,
      price = sellOrder.price,
      buyMatcherFee = buyOrder.matcherFee,
      sellMatcherFee = sellOrder.matcherFee,
      fee = 300000L,
      timestamp = nowTs,
      proofs = Proofs.empty
    )

  protected def mkSeqWsMatchTxInfo(price: Double, amount: Double): Seq[WsMatchTransactionInfo] =
    Seq(mkWsMatchTxInfo(price, amount))

  protected def mkWsMatchTxInfo(price: Double, amount: Double): WsMatchTransactionInfo =
    WsMatchTransactionInfo(ByteStr.empty, 0L, price, amount, amount * price)

}
