package com.wavesplatform.dex.history

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.google.common.base.Charsets
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1}
import com.wavesplatform.dex.history.HistoryRouterActor.HistoryInsertMsg.{SaveEvent, SaveOrder}
import com.wavesplatform.dex.model.Events._
import com.wavesplatform.dex.model.{AcceptedOrder, Events, LimitOrder}
import com.wavesplatform.dex.time.SystemTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class HistoryRouterActorSpecification
    extends TestKit(ActorSystem("HistoryRouterSpecification"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with SystemTime
    with MatcherSpecBase {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  def getKeyPair(seed: String): KeyPair = KeyPair(seed.getBytes(Charsets.UTF_8))

  val assetId: ByteStr = ByteStr("asset".getBytes)

  val assetDecimals: Byte = 8
  val wavesDecimals: Byte = 8

  val sender0Seed = "test"
  val sender1Seed = "test1"
  val sender2Seed = "test2"
  val sender3Seed = "test3"

  val buyWavesOrder = getOrder(sender0Seed, OrderType.BUY, 300L, 1L)
  val sellWavesOrder1 = getOrder(sender1Seed, OrderType.SELL, 100L, 2L)
  val sellWavesOrder2 = getOrder(sender2Seed, OrderType.SELL, 100L, 3L)
  val sellWavesOrder3 = getOrder(sender3Seed, OrderType.SELL, 100L, 4L)

  val buyWavesOrderCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 5L)

  val buyWavesOrderFilledAndCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 6L)
  val sellWavesOrder4 = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)

  val sellWavesOrderFilling = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)
  val buyWavesOrderFilledAfterPlacing = getOrder(sender0Seed, OrderType.BUY, 100L, 8L)

  def getOrder(senderSeed: String, orderType: OrderType, amount: Long, timestamp: Long): LimitOrder =
    LimitOrder(
      OrderV1(
        sender = getKeyPair(senderSeed),
        matcher = PublicKey("matcher".getBytes()),
        pair = AssetPair(Waves, IssuedAsset(assetId)),
        orderType = orderType,
        price = Order.PriceConstant,
        amount = amount * Order.PriceConstant,
        timestamp = timestamp,
        expiration = 1000L,
        matcherFee = matcherFee
      )
    )

  def orderAdded(submitted: LimitOrder): OrderAdded = OrderAdded(submitted, OrderAddedReason.RequestExecuted, time.getTimestamp())

  def orderCancelled(submitted: AcceptedOrder): OrderCanceled =
    OrderCanceled(submitted, OrderCanceledReason.RequestExecuted, time.getTimestamp())

  def orderExecuted(submitted: AcceptedOrder, counter: LimitOrder): OrderExecuted =
    OrderExecuted(submitted, counter, time.getTimestamp(), counter.matcherFee, submitted.matcherFee, 0L)

  // don't need to use blockchain in order to find out asset decimals, therefore pair parameter isn't used
  def denormalizeAmountAndFee(value: Long, asset: Asset): BigDecimal = Denormalization.denormalizeAmountAndFee(value, wavesDecimals)
  def denormalizePrice(value: Long, pair: AssetPair): BigDecimal = Denormalization.denormalizePrice(value, wavesDecimals, assetDecimals)

  implicit class LimitOrderOps(limitOrder: LimitOrder) {
    def orderId: String = limitOrder.order.id().toString
    def senderPublicKey: String = limitOrder.order.senderPublicKey.toString
  }

  case class OrderShortenedInfo(id: String, senderPublicKey: String, side: Byte, price: BigDecimal, amount: BigDecimal)

  case class EventShortenedInfo(
    orderId: String,
    eventType: Byte,
    filled: BigDecimal,
    totalFilled: BigDecimal,
    status: Byte,
    reason: EventReason = Events.NotTracked
  )

  def getOrderInfo(orderAddedEvent: OrderAdded): OrderShortenedInfo =
    SaveOrder(orderAddedEvent.order, orderAddedEvent.timestamp)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => OrderShortenedInfo(r.id, r.senderPublicKey, r.side, r.price, r.amount))
      .head

  def getEventsInfo(event: Event): Set[EventShortenedInfo] =
    SaveEvent(event)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => EventShortenedInfo(r.orderId, r.eventType, r.filled, r.totalFilled, r.status, r.reason))

  "HistoryRouter" should {
    "correctly convert events to records" in {

      import HistoryRouterActor._

      // place big buy order
      getOrderInfo(orderAdded(buyWavesOrder)) shouldBe
      OrderShortenedInfo(buyWavesOrder.orderId, buyWavesOrder.senderPublicKey, buySide, price = 1, amount = 300)

      // place small sell order 1
      getOrderInfo(orderAdded(sellWavesOrder1)) shouldBe
      OrderShortenedInfo(sellWavesOrder1.orderId, sellWavesOrder1.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed first time
      val orderExecutedEvent1 = orderExecuted(buyWavesOrder, sellWavesOrder1)
      getEventsInfo(orderExecutedEvent1) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled, OrderExecutedReason),
        EventShortenedInfo(sellWavesOrder1.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled, OrderExecutedReason)
      )

      // place small sell order 2
      getOrderInfo(orderAdded(sellWavesOrder2)) shouldBe
      OrderShortenedInfo(sellWavesOrder2.orderId, sellWavesOrder2.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed second time
      val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellWavesOrder2)
      getEventsInfo(orderExecutedEvent2) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 200, statusPartiallyFilled, OrderExecutedReason),
        EventShortenedInfo(sellWavesOrder2.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled, OrderExecutedReason)
      )

      // place small sell order 3
      getOrderInfo(orderAdded(sellWavesOrder3)) shouldBe
      OrderShortenedInfo(sellWavesOrder3.orderId, sellWavesOrder3.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed third time and filled
      val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellWavesOrder3)
      getEventsInfo(orderExecutedEvent3) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 300, statusFilled, OrderExecutedReason),
        EventShortenedInfo(sellWavesOrder3.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled, OrderExecutedReason)
      )

      // place order and then cancel
      getOrderInfo(orderAdded(buyWavesOrderCancelled)) shouldBe
      OrderShortenedInfo(buyWavesOrderCancelled.orderId, buyWavesOrderCancelled.senderPublicKey, buySide, price = 1, amount = 300)

      getEventsInfo(orderCancelled(buyWavesOrderCancelled)) shouldBe Set(
        EventShortenedInfo(
          buyWavesOrderCancelled.orderId,
          eventCancel,
          filled = 0,
          totalFilled = 0,
          statusCancelled,
          OrderCanceledReason.RequestExecuted
        )
      )

      // place buy order
      getOrderInfo(orderAdded(buyWavesOrderFilledAndCancelled)) shouldBe
      OrderShortenedInfo(
        buyWavesOrderFilledAndCancelled.orderId,
        buyWavesOrderFilledAndCancelled.senderPublicKey,
        buySide,
        price = 1,
        amount = 300
      )

      // place sell order
      getOrderInfo(orderAdded(sellWavesOrder4)) shouldBe
      OrderShortenedInfo(sellWavesOrder4.orderId, sellWavesOrder4.senderPublicKey, sellSide, price = 1, amount = 100)

      // buy order partially filled
      val cancellingOrderExecutedEvent = orderExecuted(buyWavesOrderFilledAndCancelled, sellWavesOrder4)
      getEventsInfo(cancellingOrderExecutedEvent) shouldBe Set(
        EventShortenedInfo(
          buyWavesOrderFilledAndCancelled.orderId,
          eventTrade,
          filled = 100,
          totalFilled = 100,
          statusPartiallyFilled,
          OrderExecutedReason
        ),
        EventShortenedInfo(sellWavesOrder4.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled, OrderExecutedReason)
      )

      // buy order cancelled
      getEventsInfo(orderCancelled(cancellingOrderExecutedEvent.submittedRemaining)) shouldBe Set(
        EventShortenedInfo(
          buyWavesOrderFilledAndCancelled.orderId,
          eventCancel,
          filled = 0,
          totalFilled = 100,
          statusCancelled,
          OrderCanceledReason.RequestExecuted
        )
      )
    }
  }
}
