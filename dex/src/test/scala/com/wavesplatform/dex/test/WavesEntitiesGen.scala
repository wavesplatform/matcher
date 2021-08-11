package com.wavesplatform.dex.test

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.gen._
import org.scalacheck.Gen
import org.scalatest.enablers.Emptiness

import java.nio.charset.StandardCharsets

// TODO Copy from waves-ext with some modifications
trait WavesEntitiesGen {

  implicit val optionEmptiness: Emptiness[Option[Any]] = (thing: Option[Any]) => thing.isEmpty

  val AssetIdLength = 32

  val defaultWavesFee = 300000

  val matcher: KeyPair = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))

  def issuedAssetGen: Gen[IssuedAsset] = byteArrayGen(AssetIdLength).map { xs =>
    IssuedAsset(ByteStr(xs))
  }

  val assetGen: Gen[Asset] = Gen.frequency(
    10 -> issuedAssetGen,
    1 -> Gen.const(Waves)
  )

  val keyPairGen: Gen[KeyPair] = bytes32gen.map(xs => KeyPair(ByteStr(xs)))

  val publicKeyGen: Gen[PublicKey] = keyPairGen.map(_.publicKey)

  val assetPairGen: Gen[AssetPair] = Gen.zip(issuedAssetGen, assetGen).map {
    case (asset1, Waves) => AssetPair(asset1, Waves)
    case (asset1, asset2) => if (asset1 == asset2) AssetPair(asset1, Waves) else AssetPair(asset1, asset2)
  }

  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - Order.MaxLiveTime)

  val orderSideGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)
  val orderAmountGen: Gen[Long] = Gen.choose(1, 1000L)
  val orderPriceGen: Gen[Long] = Gen.choose(1, 1000L).map(_ * Order.PriceConstant)
  val orderTtlGen: Gen[Long] = Gen.choose(1, Order.MaxLiveTime)
  val orderFeeGen: Gen[Long] = Gen.choose(1, Order.MaxAmount)

  def orderAndSenderGen(
    sideGen: Gen[OrderType] = orderSideGen,
    senderGen: Gen[KeyPair] = keyPairGen,
    matcherGen: Gen[PublicKey] = publicKeyGen,
    assetPairGen: Gen[AssetPair] = assetPairGen,
    priceGen: Gen[Long] = orderPriceGen,
    timestampGen: Gen[Long] = timestampGen,
    versionGen: Gen[Byte] = Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
  ): Gen[(Order, KeyPair)] =
    for {
      tpe <- sideGen
      sender <- senderGen
      matcher <- matcherGen
      assetPair <- assetPairGen
      amount <- orderAmountGen
      price <- priceGen
      timestamp <- timestampGen
      ttl <- orderTtlGen
      fee <- orderFeeGen
      feeAsset <- assetGen
      version <- versionGen
    } yield {
      val expiration = timestamp + ttl
      val fixedVersion = if (feeAsset == Waves) version else math.max(version, 3).toByte
      val order =
        if (tpe == OrderType.BUY) Order.buy(sender, matcher, assetPair, amount, price, timestamp, expiration, fee, fixedVersion, feeAsset)
        else Order.sell(sender, matcher, assetPair, amount, price, timestamp, expiration, fee, fixedVersion, feeAsset)
      (order, sender)
    }

  val exchangeTransactionGen: Gen[ExchangeTransaction] = {
    for {
      version <- Gen.oneOf(1: Byte, 2: Byte)
      matcher <- keyPairGen
      matcherPublicKeyGen = Gen.const(matcher.publicKey)
      timestamp <- timestampGen
      orderTimestampGen = Gen.choose(1, 1000L).map(_ + timestamp)
      (buyOrder, _) <- {
        val sideGen = Gen.const(OrderType.BUY)
        if (version == 1)
          orderAndSenderGen(
            sideGen = sideGen,
            matcherGen = matcherPublicKeyGen,
            versionGen = Gen.const(1: Byte),
            timestampGen = orderTimestampGen
          )
        else orderAndSenderGen(sideGen = sideGen, matcherGen = matcherPublicKeyGen, timestampGen = orderTimestampGen)
      }
      (sellOrder, _) <- {
        val sideGen = Gen.const(OrderType.SELL)
        val priceGen = Gen.choose(1L, buyOrder.price)
        val assetPairGen = Gen.const(buyOrder.assetPair)
        if (version == 1)
          orderAndSenderGen(
            sideGen = sideGen,
            matcherGen = matcherPublicKeyGen,
            assetPairGen = assetPairGen,
            priceGen = priceGen,
            versionGen = Gen.const(1: Byte),
            timestampGen = orderTimestampGen
          )
        else
          orderAndSenderGen(
            sideGen = sideGen,
            matcherGen = matcherPublicKeyGen,
            assetPairGen = assetPairGen,
            priceGen = priceGen,
            timestampGen = orderTimestampGen
          )
      }
      fee <- orderFeeGen
    } yield {
      val amount = math.min(buyOrder.amount, sellOrder.amount)
      val price = buyOrder.price
      ExchangeTransactionV2
        .create(
          matcher = matcher,
          buyOrder = buyOrder,
          sellOrder = sellOrder,
          amount = amount,
          price = price,
          buyMatcherFee = buyOrder.matcherFee,
          sellMatcherFee = sellOrder.matcherFee,
          fee = fee,
          timestamp = timestamp
        ).transaction
    }
  }

  protected def exchangeTransactionBuyerSellerGen(buyerGen: Gen[KeyPair], sellerGen: Gen[KeyPair]): Gen[ExchangeTransactionV2] = for {
    matcher <- keyPairGen
    matcherPublicKeyGen = Gen.const(matcher.publicKey)
    timestamp <- timestampGen
    orderTimestampGen = Gen.choose(1, 1000L).map(_ + timestamp)
    (buyOrder, _) <- orderAndSenderGen(
      sideGen = Gen.const(OrderType.BUY),
      senderGen = buyerGen,
      matcherGen = matcherPublicKeyGen,
      timestampGen = orderTimestampGen
    )
    (sellOrder, _) <- orderAndSenderGen(
      sideGen = Gen.const(OrderType.SELL),
      senderGen = sellerGen,
      matcherGen = matcherPublicKeyGen,
      assetPairGen = Gen.const(buyOrder.assetPair),
      priceGen = Gen.choose(1L, buyOrder.price),
      timestampGen = orderTimestampGen
    )
  } yield {
    val amount = math.min(buyOrder.amount, sellOrder.amount)
    val price = buyOrder.price
    ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buyOrder,
        sellOrder = sellOrder,
        amount = amount,
        price = price,
        buyMatcherFee = buyOrder.matcherFee,
        sellMatcherFee = sellOrder.matcherFee,
        fee = defaultWavesFee,
        timestamp = timestamp
      ).transaction
  }

}
