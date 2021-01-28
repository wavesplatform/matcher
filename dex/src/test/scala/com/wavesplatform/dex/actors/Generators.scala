package com.wavesplatform.dex.actors

import com.wavesplatform.dex.domain.account.KeyPair.toPublicKey
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.gen.bytes32gen
import com.wavesplatform.dex.model.Events.OrderCanceledReason
import com.wavesplatform.dex.model.{Events, LimitOrder}
import com.wavesplatform.dex.test.WavesEntitiesGen
import org.scalacheck.Gen

trait Generators extends WavesEntitiesGen {
  protected val definedAssets: List[Asset] = Asset.Waves :: Gen.listOfN(2, assetGen).sample.get

  protected val definedAssetsGen: Gen[Asset] = Gen.oneOf(definedAssets)

  protected val addressGen: Gen[Address] = keyPairGen.map(_.toAddress)

  protected val txIdGen: Gen[ExchangeTransaction.Id] = bytes32gen.map(ByteStr(_))

  protected val balancesGen: Gen[Map[Asset, Long]] = Gen.choose(0, definedAssets.size).flatMap { size =>
    Gen.mapOfN(size, Gen.zip(definedAssetsGen, Gen.choose(0, 10L)))
  }

  // With a constant price!
  protected def executedEventGen(counterGen: Gen[KeyPair] = keyPairGen, submitterGen: Gen[KeyPair] = keyPairGen): Gen[Events.OrderExecuted] =
    for {
      assetPair <- assetPairGen
      now = Gen.const(System.currentTimeMillis())
      sellPrice <- orderPriceGen
      (counter, _) <- orderAndSenderGen(
        sideGen = Gen.const(OrderType.SELL),
        senderGen = counterGen,
        matcherGen = toPublicKey(matcher),
        assetPairGen = Gen.const(assetPair),
        priceGen = Gen.const(sellPrice),
        timestampGen = now
      )
      (submitted, _) <- orderAndSenderGen(
        sideGen = Gen.const(OrderType.BUY),
        senderGen = submitterGen,
        matcherGen = toPublicKey(matcher),
        assetPairGen = Gen.const(assetPair),
        priceGen = Gen.choose(sellPrice, 1000L * Order.PriceConstant),
        timestampGen = now
      )
    } yield Events.OrderExecuted(
      submitted = LimitOrder(submitted),
      counter = LimitOrder(counter),
      timestamp = submitted.timestamp,
      counterExecutedFee = counter.matcherFee,
      submittedExecutedFee = submitted.matcherFee
    )

  protected def canceledEventGen(senderGen: Gen[KeyPair] = keyPairGen): Gen[Events.OrderCanceled] =
    for {
      (order, _) <- orderAndSenderGen(senderGen = senderGen, matcherGen = toPublicKey(matcher))
    } yield Events.OrderCanceled(
      acceptedOrder = LimitOrder(order),
      reason = OrderCanceledReason.BecameInvalid,
      timestamp = order.timestamp
    )

  protected val eventGen: Gen[Events.Event] = Gen.oneOf(executedEventGen(), canceledEventGen())

}
