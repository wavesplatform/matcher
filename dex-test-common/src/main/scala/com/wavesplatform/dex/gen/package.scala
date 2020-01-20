package com.wavesplatform.dex

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.waves.WavesFeeConstants.matcherFee
import org.scalacheck.Gen

package object gen {
  def orderGen(matcher: PublicKey,
               trader: KeyPair,
               assetPairs: Seq[AssetPair],
               types: Seq[OrderType] = Seq(OrderType.BUY, OrderType.SELL)): Gen[Order] =
    for {
      assetPair      <- Gen.oneOf(assetPairs)
      tpe            <- Gen.oneOf(types)
      amount         <- Gen.choose(10, 100)
      price          <- Gen.choose(10, 100)
      orderVersion   <- Gen.oneOf(1: Byte, 2: Byte)
      expirationDiff <- Gen.choose(600000, 6000000)
    } yield {
      val ts = System.currentTimeMillis()
      if (tpe == OrderType.BUY)
        Order.buy(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
      else
        Order.sell(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
    }
}
