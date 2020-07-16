package com.wavesplatform.dex.test

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.gen._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange._
import org.scalacheck.Gen

trait WavesEntitiesGen {
  val AssetIdLength = 32

  def issuedAssetGen: Gen[IssuedAsset] = byteArrayGen(AssetIdLength).map { xs =>
    IssuedAsset(ByteStr(xs))
  }

  val assetGen: Gen[Asset] = Gen.frequency(
    10 -> issuedAssetGen,
    1  -> Gen.const(Waves)
  )

  val keyPairGen: Gen[KeyPair]     = bytes32gen.map(xs => KeyPair(ByteStr(xs)))
  val publicKeyGen: Gen[PublicKey] = keyPairGen.map(_.publicKey)
  val assetPairGen: Gen[AssetPair] = Gen.zip(issuedAssetGen, assetGen).map {
    case (asset1, Waves)  => AssetPair(asset1, Waves)
    case (asset1, asset2) => if (asset1 == asset2) AssetPair(asset1, Waves) else AssetPair(asset1, asset2)
  }
  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - Order.MaxLiveTime)

  val orderSideGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)
  val orderAmountGen: Gen[Long]    = Gen.choose(1, 1000L)
  val orderPriceGen: Gen[Long]     = Gen.choose(1, 1000L).map(_ * Order.PriceConstant)
  val orderTtlGen: Gen[Long]       = Gen.choose(1, Order.MaxLiveTime)
  val orderFeeGen: Gen[Long]       = Gen.choose(1, Order.MaxAmount)

  def orderAndSenderGen(sideGen: Gen[OrderType] = orderSideGen,
                        matcherGen: Gen[PublicKey] = publicKeyGen,
                        assetPairGen: Gen[AssetPair] = assetPairGen,
                        priceGen: Gen[Long] = orderPriceGen,
                        timestampGen: Gen[Long] = timestampGen,
                        versionGen: Gen[Byte] = Gen.oneOf(1: Byte, 2: Byte, 3: Byte)): Gen[(Order, KeyPair)] =
    for {
      tpe       <- sideGen
      sender    <- keyPairGen
      matcher   <- matcherGen
      assetPair <- assetPairGen
      amount    <- orderAmountGen
      price     <- priceGen
      timestamp <- timestampGen
      ttl       <- orderTtlGen
      fee       <- orderFeeGen
      feeAsset  <- assetGen
      version   <- versionGen
    } yield {
      val expiration   = timestamp + ttl
      val fixedVersion = if (feeAsset == Waves) version else math.max(version, 3).toByte
      val order =
        if (tpe == OrderType.BUY) Order.buy(fixedVersion, sender, matcher, assetPair, amount, price, timestamp, expiration, fee, feeAsset)
        else Order.sell(fixedVersion, sender, matcher, assetPair, amount, price, timestamp, expiration, fee, feeAsset)
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
          orderAndSenderGen(sideGen = sideGen, matcherGen = matcherPublicKeyGen, versionGen = Gen.const(1: Byte), timestampGen = orderTimestampGen)
        else orderAndSenderGen(sideGen = sideGen, matcherGen = matcherPublicKeyGen, timestampGen = orderTimestampGen)
      }
      (sellOrder, _) <- {
        val sideGen      = Gen.const(OrderType.SELL)
        val priceGen     = Gen.choose(1L, buyOrder.price)
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
          orderAndSenderGen(sideGen = sideGen,
                            matcherGen = matcherPublicKeyGen,
                            assetPairGen = assetPairGen,
                            priceGen = priceGen,
                            timestampGen = orderTimestampGen)
      }
      fee <- orderFeeGen
    } yield {
      val amount       = math.min(buyOrder.amount, sellOrder.amount)
      val price        = buyOrder.price
      val fixedVersion = if (buyOrder.matcherFeeAssetId == Waves && sellOrder.matcherFeeAssetId == Waves) version else math.max(version, 2).toByte
      ExchangeTransaction
        .create(
          version = fixedVersion,
          order1 = buyOrder,
          order2 = sellOrder,
          amount = amount,
          price = price,
          buyMatcherFee = buyOrder.matcherFee,
          sellMatcherFee = sellOrder.matcherFee,
          fee = fee,
          timestamp = timestamp
        )
        .map(_.signWith(matcher.privateKey))
        .explicitGet()
    }
  }
}
