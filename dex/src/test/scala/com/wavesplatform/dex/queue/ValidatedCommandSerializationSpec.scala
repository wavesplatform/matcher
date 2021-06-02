package com.wavesplatform.dex.queue

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.MatcherSpec
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{OrderType, OrderV1}
import com.wavesplatform.dex.queue.ValidatedCommand.CancelOrder

import scala.concurrent.duration._

//TODO add more tests

final class ValidatedCommandSerializationSpec extends MatcherSpec with MatcherSpecBase {

  "ValidatedCommandSerializationSpec" should {

    "write & read cancel order command with owner" in {
      val cancelOrder = CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, Some(order.sender.toAddress))
      val parsedCancelOrder = ValidatedCommand.fromBytes(ValidatedCommand.toBytes(cancelOrder))
      parsedCancelOrder shouldBe cancelOrder
    }

    "write & read cancel order command without owner" in {
      val cancelOrder = CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, None)
      val parsedCancelOrder = ValidatedCommand.fromBytes(ValidatedCommand.toBytes(cancelOrder))
      parsedCancelOrder shouldBe cancelOrder
    }
  }

  private lazy val WUSD = IssuedAsset(ByteStr.decodeBase58("HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj").get)

  private lazy val order = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, WUSD),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = System.currentTimeMillis(),
    expiration = System.currentTimeMillis() + 5.days.toMillis,
    matcherFee = matcherFee
  )

}
