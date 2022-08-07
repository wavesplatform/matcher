package com.wavesplatform.dex.test

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
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

}
