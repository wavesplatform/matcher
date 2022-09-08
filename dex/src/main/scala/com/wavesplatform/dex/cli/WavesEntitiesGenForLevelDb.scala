package com.wavesplatform.dex.cli

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.{AcceptedOrderType, OrderInfo, OrderStatus}
import org.scalacheck.{Arbitrary, Gen}

import java.math.BigInteger
import java.nio.charset.StandardCharsets

// copied from com.wavesplatform.dex.test.WavesEntitiesGen
object WavesEntitiesGenForLevelDb {

  val AssetIdLength = 32

  val defaultWavesFee = 300000

  val matcher: KeyPair = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))

  protected def assertDescriptionsGen(n: Int): Gen[Map[Asset.IssuedAsset, BriefAssetDescription]] =
    Gen.containerOfN[Seq, (Asset.IssuedAsset, BriefAssetDescription)](n, assertDescriptionGen).map(_.toMap)

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val issuedAssetIdGen: Gen[Array[Byte]] = bytes32gen
  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  protected val assertDescriptionGen: Gen[(Asset.IssuedAsset, BriefAssetDescription)] = for {
    asset <- issuedAssetGen(1.toByte)
    name <- Arbitrary.arbString.arbitrary
    decimals <- Gen.choose(0, 8)
    hasScript <- Arbitrary.arbBool.arbitrary
    isNft <- Gen.oneOf(true, false)
  } yield (asset, BriefAssetDescription(name, decimals, hasScript, isNft))

  def issuedAssetGen(prefix: Byte): Gen[IssuedAsset] =
    Gen
      .listOfN(Asset.AssetIdLength - 1, Arbitrary.arbitrary[Byte])
      .map(xs => IssuedAsset(ByteStr(Array(prefix, xs: _*))))

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

  def orderAndOrderInfoGen(
    sideGen: Gen[OrderType] = orderSideGen,
    senderGen: Gen[KeyPair] = keyPairGen,
    assetPairGen: Gen[AssetPair] = assetPairGen,
    priceGen: Gen[Long] = orderPriceGen,
    timestampGen: Gen[Long] = timestampGen,
    versionGen: Gen[Byte] = Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
  ): Gen[(Order, OrderInfo[OrderStatus.Final])] =
    for {
      tpe <- sideGen
      sender <- senderGen
      assetPair <- assetPairGen
      amount <- orderAmountGen
      price <- priceGen
      timestamp <- timestampGen
      ttl <- orderTtlGen
      fee <- orderFeeGen
      feeAsset <- assetGen
      version <- versionGen
      order = mkOrder(timestamp, ttl, feeAsset, version, tpe, sender, assetPair, amount, price, fee)
      orderInfo <- finalizedOrderInfoGen(order)
    } yield (order, orderInfo)

  private def mkOrder(
    timestamp: Long,
    ttl: Long,
    feeAsset: Asset,
    version: Byte,
    tpe: OrderType,
    sender: KeyPair,
    assetPair: AssetPair,
    amount: Amount,
    price: Price,
    fee: Long
  ): Order = {
    val expiration = timestamp + ttl
    val fixedVersion = if (feeAsset == Waves) version else math.max(version, 3).toByte
    if (tpe == OrderType.BUY) Order.buy(sender, matcher, assetPair, amount, price, timestamp, expiration, fee, fixedVersion, feeAsset)
    else Order.sell(sender, matcher, assetPair, amount, price, timestamp, expiration, fee, fixedVersion, feeAsset)
  }

  private def finalizedOrderInfoGen(o: Order): Gen[OrderInfo[OrderStatus.Final]] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      filledFee <- Gen.choose(0, o.matcherFee)
      status <- Gen.oneOf(OrderStatus.Filled(filledAmount, filledFee), OrderStatus.Cancelled(filledAmount, filledFee))
      aoType <- Gen.oneOf(AcceptedOrderType.Limit, AcceptedOrderType.Market)
      avgWeighedPrice <- Gen.choose(0, o.price)
    } yield OrderInfo
      .v6[OrderStatus.Final](
        o.orderType,
        o.amount,
        o.price,
        o.matcherFee,
        o.feeAsset,
        o.timestamp,
        status,
        o.assetPair,
        aoType,
        o.version,
        getAvgWeighedPriceNominator(filledAmount, avgWeighedPrice)
      )

  private def getAvgWeighedPriceNominator(filledAmount: Long, avgWeighedPrice: Long): BigInteger =
    BigInteger.valueOf(filledAmount).multiply(BigInteger.valueOf(avgWeighedPrice))

}
