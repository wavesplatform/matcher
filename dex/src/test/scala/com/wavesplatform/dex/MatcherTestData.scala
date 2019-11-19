package com.wavesplatform.dex

import java.util.concurrent.atomic.AtomicLong

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.AssetsDB
import com.wavesplatform.dex.db.AssetsDB.Item
import com.wavesplatform.dex.effect.FutureResult
import com.wavesplatform.dex.model.MatcherModel.{Normalization, Price}
import com.wavesplatform.dex.model.OrderValidator.Result
import com.wavesplatform.dex.model.{BuyLimitOrder, LimitOrder, OrderValidator, SellLimitOrder, _}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.OrderFeeSettings._
import com.wavesplatform.dex.settings.{AssetType, MatcherSettings}
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV3}
import com.wavesplatform.{NTPTime, RequestGen, crypto => wcrypto}
import mouse.any._
import net.ceedubs.ficus.Ficus._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

trait MatcherTestData extends RequestGen with NTPTime { _: Suite =>
  private val signatureSize = 32

  val WalletSeed               = ByteStr("Matcher".getBytes("utf-8"))
  val MatcherSeed: Array[Byte] = wcrypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount           = KeyPair(MatcherSeed)
  val senderKeyPair            = KeyPair("seed".getBytes("utf-8"))

  private val seqNr           = new AtomicLong(-1)
  val defaultAssetDescription = AssetsDB.Item("Asset", 8)

  val btc: IssuedAsset = mkAssetId("WBTC")
  val usd: IssuedAsset = mkAssetId("WUSD")
  val eth: IssuedAsset = mkAssetId("WETH")

  val pairWavesBtc = AssetPair(Waves, btc)
  val pairWavesUsd = AssetPair(Waves, usd)

  val defaultAssetDescriptionsMap: Map[Asset, Item] = {
    Map[Asset, AssetsDB.Item](usd -> Item("USD", 2), btc -> Item("BTC", 8)).withDefaultValue(defaultAssetDescription)
  }

  val getDefaultAssetDescriptions: Asset => AssetsDB.Item = defaultAssetDescriptionsMap.apply

  def getDefaultAssetDescriptions(asset: Asset, description: AssetsDB.Item): Asset => AssetsDB.Item = {
    defaultAssetDescriptionsMap ++ Map(asset -> description)
  }

  def getDefaultAssetDescriptions(assetAndDesc: (Asset, AssetsDB.Item)*): Asset => AssetsDB.Item = {
    defaultAssetDescriptionsMap ++ Map(assetAndDesc: _*)
  }

  val rateCache: RateCache = RateCache.inMem unsafeTap { _.upsertRate(usd, 3.7) } unsafeTap { _.upsertRate(btc, 0.00011167) }

  implicit class DoubleOps(value: Double) {
    val waves, btc, eth = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long       = Normalization.normalizeAmountAndFee(value, 2)
  }

  def toNormalized(value: Long): Long = value * Order.PriceConstant

  def wrapLimitOrder(x: Order): QueueEventWithMeta                  = wrapLimitOrder(seqNr.incrementAndGet(), x)
  def wrapLimitOrder(n: Long, x: Order): QueueEventWithMeta         = wrapEvent(n, QueueEvent.Placed(LimitOrder(x)))
  def wrapEvent(n: Long, event: QueueEvent): QueueEventWithMeta     = QueueEventWithMeta(n, System.currentTimeMillis(), event)
  def wrapMarketOrder(mo: MarketOrder): QueueEventWithMeta          = wrapEvent(seqNr.incrementAndGet(), QueueEvent.PlacedMarket(mo))
  def wrapMarketOrder(n: Long, mo: MarketOrder): QueueEventWithMeta = wrapEvent(n, QueueEvent.PlacedMarket(mo))

  val smallFee = Some(toNormalized(1))

  def awaitResult[A](result: FutureResult[A]): Result[A]            = Await.result(result.value, Duration.Inf)
  def awaitResult[A, B](result: Future[Either[A, B]]): Either[A, B] = Await.result(result, Duration.Inf)

  def getSpentAmountWithFee(order: Order): Long = {
    val lo = LimitOrder(order)
    lo.spentAmount + (if (order.getSpendAssetId == order.matcherFeeAssetId) lo.fee else 0)
  }

  def createOrder(pair: AssetPair,
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
      price = Normalization.normalizePrice(price,
                                           getDefaultAssetDescriptions(pair.amountAsset).decimals,
                                           getDefaultAssetDescriptions(pair.priceAsset).decimals),
      timestamp = ntpNow,
      expiration = ntpNow + (1000 * 60 * 60 * 24),
      matcherFee = matcherFee,
      version = version,
      matcherFeeAssetId = matcherFeeAsset
    )
  }

  def assetIdGen(prefix: Byte): Gen[IssuedAsset] =
    Gen
      .listOfN(signatureSize - 1, Arbitrary.arbitrary[Byte])
      .map(xs => IssuedAsset(ByteStr(Array(prefix, xs: _*))))

  def arbitraryAssetIdGen: Gen[IssuedAsset] = assetIdGen(Random.nextInt(Byte.MaxValue).toByte)

  val distinctPairGen: Gen[AssetPair] = for {
    a1 <- assetIdGen(1.toByte)
    a2 <- assetIdGen(2.toByte)
  } yield AssetPair(a1, a2)

  protected def mkAssetId(prefix: String): IssuedAsset = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    IssuedAsset(ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32)))
  }

  override val assetPairGen: Gen[AssetPair] = {
    Gen.frequency((18, distinctPairGen), (1, assetIdGen(1).map(AssetPair(_, Waves))), (1, assetIdGen(2).map(AssetPair(Waves, _))))
  }

  val maxTimeGen: Gen[Long]     = Gen.choose(10000L, Order.MaxLiveTime).map(_ + System.currentTimeMillis())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(System.currentTimeMillis() - _)

  val config: Config = loadConfig(ConfigFactory.parseString("""waves {
      |  directory: "/tmp/waves-test"
      |  dex {
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    order-history-file: null
      |    min-order-fee: 100000
      |    snapshots-interval: 100000
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2", "BASE"]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[Ff]orbidden"]
      |    allowed-order-versions = [1, 2, 3]
      |  }
      |}""".stripMargin))

  val matcherSettings: MatcherSettings = config.as[MatcherSettings]("waves.dex")

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxWavesAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair,
                   amount: Long,
                   price: Long,
                   sender: Option[KeyPair] = None,
                   matcherFee: Option[Long] = None,
                   version: Byte = 1,
                   timestamp: Option[Long],
                   feeAsset: Asset = Waves): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair  <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long  <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version, feeAsset), sender)

  def sellGenerator(pair: AssetPair,
                    amount: Price,
                    price: Price,
                    sender: Option[KeyPair] = None,
                    matcherFee: Option[Price] = None,
                    timestamp: Option[Price],
                    version: Byte = 1,
                    feeAsset: Asset = Waves): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair  <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long  <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxWavesAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version, feeAsset), sender)

  def buy(pair: AssetPair,
          amount: Price,
          price: BigDecimal,
          sender: Option[KeyPair] = None,
          matcherFee: Option[Long] = None,
          ts: Option[Long] = None,
          version: Byte = 1,
          feeAsset: Asset = Waves): Order = rawBuy(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version, feeAsset)

  def rawBuy(pair: AssetPair,
             amount: Long,
             price: Price,
             sender: Option[KeyPair] = None,
             matcherFee: Option[Long] = None,
             ts: Option[Long] = None,
             version: Byte = 1,
             feeAsset: Asset = Waves): Order =
    valueFromGen(buyGenerator(pair, amount, price, sender, matcherFee, version, ts, feeAsset))._1

  def sell(pair: AssetPair,
           amount: Long,
           price: BigDecimal,
           sender: Option[KeyPair] = None,
           matcherFee: Option[Long] = None,
           ts: Option[Long] = None,
           version: Byte = 1,
           feeAsset: Asset = Waves): Order = rawSell(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version, feeAsset)

  def rawSell(pair: AssetPair,
              amount: Long,
              price: Price,
              sender: Option[KeyPair] = None,
              matcherFee: Option[Long] = None,
              ts: Option[Long] = None,
              version: Byte = 1,
              feeAsset: Asset = Waves): Order =
    valueFromGen(sellGenerator(pair, amount, price, sender, matcherFee, ts, version, feeAsset))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  def orderGenerator(sender: KeyPair, pair: AssetPair): Gen[Order] =
    for {
      orderType          <- orderTypeGenerator
      amount: Long       <- maxWavesAmountGen
      price: Long        <- Gen.choose((BigDecimal(10).pow(8) / amount).toLong.max(1), (Long.MaxValue / amount) - 100)
      timestamp: Long    <- createdTimeGen
      expiration: Long   <- maxTimeGen
      matcherFee: Long   <- maxWavesAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset     <- arbitraryAssetIdGen
      matcherFeeAssetId  <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      if (orderVersion == 3)
        Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion, matcherFeeAssetId)
      else Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion)
    }

  val orderGenerator: Gen[(Order, KeyPair)] = for {
    sender: KeyPair <- accountGen
    pair            <- assetPairGen
    order           <- orderGenerator(sender, pair)
  } yield order -> sender

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: KeyPair    <- accountGen
    pair               <- assetPairGen
    amount: Long       <- maxWavesAmountGen
    price: Long        <- maxWavesAmountGen
    timestamp: Long    <- createdTimeGen
    expiration: Long   <- maxTimeGen
    matcherFee: Long   <- maxWavesAmountGen
    orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
  } yield BuyLimitOrder(amount, matcherFee, Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: KeyPair    <- accountGen
    pair               <- assetPairGen
    amount: Long       <- maxWavesAmountGen
    price: Long        <- maxWavesAmountGen
    timestamp: Long    <- createdTimeGen
    expiration: Long   <- maxTimeGen
    matcherFee: Long   <- maxWavesAmountGen
    orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
  } yield SellLimitOrder(amount, matcherFee, Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val limitOrderGenerator: Gen[LimitOrder] = Gen.oneOf(sellLimitOrderGenerator, buyLimitOrderGenerator)

  val orderV3Generator: Gen[Order] =
    for {
      sender: KeyPair   <- accountGen
      pair              <- assetPairGen
      orderType         <- orderTypeGenerator
      amount: Long      <- maxWavesAmountGen
      price: Long       <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long   <- createdTimeGen
      expiration: Long  <- maxTimeGen
      matcherFee: Long  <- maxWavesAmountGen
      arbitraryAsset    <- arbitraryAssetIdGen
      matcherFeeAssetId <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      OrderV3(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    }

  def orderV3WithPredefinedFeeAssetGenerator(matcherFeeAsset: Option[Asset] = None): Gen[(KeyPair, Order)] = {
    for {
      sender: KeyPair  <- accountGen
      pair             <- distinctPairGen
      orderType        <- orderTypeGenerator
      amount: Long     <- maxWavesAmountGen
      price: Long      <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long  <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxWavesAmountGen
      arbitraryAsset   <- arbitraryAssetIdGen
    } yield {

      sender -> OrderV3(sender,
                        MatcherAccount,
                        pair,
                        orderType,
                        amount,
                        price,
                        timestamp,
                        expiration,
                        matcherFee,
                        matcherFeeAsset getOrElse arbitraryAsset)
    }
  }

  val orderV3PairGenerator: Gen[((KeyPair, Order), (KeyPair, Order))] =
    for {
      senderBuy: KeyPair   <- accountGen
      senderSell: KeyPair  <- accountGen
      pair                 <- assetPairGen
      amount: Long         <- maxWavesAmountGen
      price: Long          <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestampBuy: Long   <- createdTimeGen
      timestampSell: Long  <- createdTimeGen
      expirationBuy: Long  <- maxTimeGen
      expirationSell: Long <- maxTimeGen
      matcherFeeBuy: Long  <- maxWavesAmountGen
      matcherFeeSell: Long <- maxWavesAmountGen
      arbitraryAsset       <- arbitraryAssetIdGen
      matcherFeeAssetId    <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Waves, arbitraryAsset)
    } yield {
      (
        senderBuy -> OrderV3(senderBuy,
                             MatcherAccount,
                             pair,
                             OrderType.BUY,
                             Order.correctAmount(amount, price),
                             price,
                             timestampBuy,
                             expirationBuy,
                             matcherFeeBuy,
                             matcherFeeAssetId),
        senderSell -> OrderV3(senderSell,
                              MatcherAccount,
                              pair,
                              OrderType.SELL,
                              Order.correctAmount(amount, price),
                              price,
                              timestampSell,
                              expirationSell,
                              matcherFeeSell,
                              matcherFeeAssetId)
      )
    }

  val percentSettingsGenerator: Gen[PercentSettings] =
    for {
      assetType <- Gen.oneOf(AssetType.values.toSeq)
      minFee    <- Gen.choose(0.01, 100.0)
    } yield PercentSettings(assetType, minFee)

  def fixedSettingsGenerator(defaultAsset: Asset, lowerMinFeeBound: Long = 1, upperMinFeeBound: Long = 1000000L): Gen[FixedSettings] =
    for { minFee <- Gen.choose(lowerMinFeeBound, upperMinFeeBound) } yield { FixedSettings(defaultAsset, minFee) }

  def dynamicSettingsGenerator(lowerBaseFeeBound: Long = 1, upperBaseFeeBound: Long = 1000000L): Gen[DynamicSettings] =
    for { baseFee <- Gen.choose(lowerBaseFeeBound, upperBaseFeeBound) } yield { DynamicSettings(baseFee) }

  def orderFeeSettingsGenerator(defaultAssetForFixedSettings: Option[Asset] = None): Gen[OrderFeeSettings] = {
    for {
      defaultAsset     <- defaultAssetForFixedSettings.fold(arbitraryAssetIdGen)(_.fold(arbitraryAssetIdGen)(Gen.const))
      orderFeeSettings <- Gen.oneOf(dynamicSettingsGenerator(), fixedSettingsGenerator(defaultAsset), percentSettingsGenerator)
    } yield orderFeeSettings
  }

  val orderWithoutWavesInPairAndWithFeeSettingsGenerator: Gen[(Order, KeyPair, OrderFeeSettings)] = {
    for {
      sender: KeyPair  <- accountGen
      amountAsset      <- arbitraryAssetIdGen
      priceAsset       <- arbitraryAssetIdGen
      orderFeeSettings <- orderFeeSettingsGenerator()
      order            <- orderGenerator(sender, AssetPair(amountAsset, priceAsset))
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  val orderV3WithFeeSettingsGenerator: Gen[(Order, OrderFeeSettings)] = {
    for {
      (sender, order)  <- orderV3WithPredefinedFeeAssetGenerator()
      orderFeeSettings <- orderFeeSettingsGenerator(Some(order.matcherFeeAssetId))
    } yield {
      correctOrderByFeeSettings(order, sender, orderFeeSettings) -> orderFeeSettings
    }
  }

  private def correctOrderByFeeSettings(order: Order,
                                        sender: KeyPair,
                                        orderFeeSettings: OrderFeeSettings,
                                        matcherFeeAssetForDynamicSettings: Option[Asset] = None,
                                        rateForDynamicSettings: Option[Double] = None): Order = {
    val correctedOrder = (order.version, orderFeeSettings) match {
      case (3, FixedSettings(defaultAssetId, minFee)) =>
        order
          .updateMatcherFeeAssetId(defaultAssetId)
          .updateFee(minFee)
      case (3, percentSettings: PercentSettings) =>
        order
          .updateMatcherFeeAssetId(OrderValidator.getValidFeeAssetForSettings(order, percentSettings, rateCache).head)
          .updateFee {
            OrderValidator.getMinValidFeeForSettings(order, percentSettings, getDefaultAssetDescriptions(_).decimals, rateCache).explicitGet()
          }
      case (_, DynamicSettings(baseFee)) =>
        order
          .updateMatcherFeeAssetId(matcherFeeAssetForDynamicSettings getOrElse Waves)
          .updateFee(
            rateForDynamicSettings.fold(baseFee) { rate =>
              OrderValidator.multiplyFeeByDouble(baseFee, rate)
            }
          )
      case _ => order
    }

    Order.sign(correctedOrder, sender)
  }
}
