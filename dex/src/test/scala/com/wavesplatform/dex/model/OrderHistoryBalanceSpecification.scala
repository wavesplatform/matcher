package com.wavesplatform.dex.model

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.address.AddressActor
import com.wavesplatform.dex.actors.address.AddressActor.OrderListType
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderAddedReason, OrderCanceled}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.time.SystemTime
import org.scalactic.source
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatest.concurrent.ScalaFutures._

import java.math.BigInteger
import scala.concurrent.duration._
import scala.reflect.ClassTag

class OrderHistoryBalanceSpecification
    extends TestKit(ActorSystem())
    with AnyPropSpecLike
    with Matchers
    with MatcherSpecBase
    with BeforeAndAfterEach
    with DiffMatcherWithImplicits
    with SystemTime
    with Eventually {

  import OrderHistoryBalanceSpecification._

  implicit override def patienceConfig: PatienceConfig = PatienceConfig(2.seconds, 5.millis)

  private val WctBtc = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
  private val WavesBtc = AssetPair(Waves, mkAssetId("BTC"))

  private def mkOrderHistory = new OrderHistoryStub(system, time, MaxActiveOrders, MaxFinalizedOrders)
  private var oh = mkOrderHistory

  override def beforeEach(): Unit = {
    super.beforeEach()
    oh = mkOrderHistory
  }

  def openVolume(address: Address, asset: Asset): Long = oh.ref(address).openVolume(asset)

  def activeOrderIds(sender: Address): Vector[Id] = oh.ref(sender).activeOrderIds
  def allOrderIds(sender: Address): Vector[Id] = oh.ref(sender).allOrderIds
  def activeOrderIdsByPair(sender: Address, pair: AssetPair): Vector[Id] = oh.ref(sender).activeOrderIdsByPair(pair)
  def allOrderIdsByPair(sender: Address, pair: AssetPair): Vector[Id] = oh.ref(sender).allOrderIdsByPair(pair)
  def orderStatus(orderId: ByteStr): OrderStatus = oh.ref(orderId).orderStatus(orderId)

  property("New buy order added") {
    val ord = buy(WctBtc, 10000, 0.0007)

    val lo = LimitOrder(ord, None, None)
    oh.process(OrderAdded(lo, OrderAddedReason.RequestExecuted, time.getTimestamp()))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 7L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("New sell order added") {
    val ord = sell(WctBtc, 10000, 0.0007)

    oh.process(OrderAdded(LimitOrder(ord, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 10000L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("New buy WAVES order added") {
    val ord = buy(WavesBtc, 1000, 0.008, matcherFee = Some(3000))
    val lo = LimitOrder(ord, None, None)

    oh.process(OrderAdded(lo, OrderAddedReason.RequestExecuted, time.getTimestamp()))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets not considering amount of received WAVES") {
      openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 3000L
      openVolume(ord.senderPublicKey, WavesBtc.priceAsset) shouldBe 8L
    }

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("New sell WAVES order added") {
    val ord = sell(WavesBtc, 10000, 0.0008)
    val lo = LimitOrder(ord, None, None)

    oh.process(OrderAdded(lo, OrderAddedReason.RequestExecuted, time.getTimestamp()))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 10000L + ord.matcherFee
    openVolume(ord.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("Should reserve fee, even if seller receives more WAVES than total fee in sell order") {
    val pair = AssetPair(mkAssetId("BTC"), Waves)
    val ord = sell(pair, 100000, 0.01, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted

    openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 1000L
  }

  property("Should reserve fee, even if buyer receives more WAVES than total fee in buy order") {
    val ord = buy(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted

    openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 1000L
  }

  property("Two sell orders added") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val ord1 = sell(WavesBtc, 10000, 0.0005, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis()))
    val ord2 = sell(WavesBtc, 16000, 0.0008, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis() + 1))

    oh.processAll(
      OrderAdded(LimitOrder(ord1, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()),
      OrderAdded(LimitOrder(ord2, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp())
    )

    withClue("all orders accepted") {
      orderStatus(ord1.id()) shouldBe OrderStatus.Accepted
      orderStatus(ord2.id()) shouldBe OrderStatus.Accepted
    }

    withClue("correction was used to reserve assets") {
      openVolume(ord1.senderPublicKey, WavesBtc.amountAsset) shouldBe ord1.amount + ord1.matcherFee + ord2.amount + ord2.matcherFee
      openVolume(ord1.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list") {
      val expected = Seq(ord2.id(), ord1.id())

      activeOrderIds(ord1.senderPublicKey) shouldBe expected
      allOrderIds(ord1.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord1.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(ord1.senderPublicKey, WavesBtc) shouldBe expected
    }
  }

  property("Buy WAVES order filled exactly") {
    val counter = buy(WavesBtc, 100000, 0.0008, matcherFee = Some(2000L))
    val submitted = sell(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    oh.process(OrderAdded(LimitOrder(submitted, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))

    val exec = mkOrderExecutedRaw(submitted, counter)
    oh.process(exec)

    withClue("executed exactly") {
      exec.executedAmount shouldBe counter.amount
      orderStatus(counter.id()) shouldBe OrderStatus.Filled(exec.executedAmount, exec.counterExecutedFee)
      orderStatus(submitted.id()) shouldBe OrderStatus.Filled(exec.executedAmount, exec.submittedExecutedFee)
    }

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"orders list of counter owner ${counter.sender}") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe Seq(counter.id())
    }

    withClue(s"orders list of submitted owner ${submitted.sender}") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe Seq(submitted.id())
    }
  }

  property("Buy WAVES order filled with remainder") {
    val counter = sell(WavesBtc, 840340L, 0.00000238, matcherFee = Some(300000L))
    val submitted = buy(WavesBtc, 425532L, 0.00000238, matcherFee = Some(300000L))

    val counterLo = LimitOrder(counter, None, None)
    oh.process(OrderAdded(counterLo, OrderAddedReason.RequestExecuted, time.getTimestamp()))
    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
      activeOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())
    }

    oh.process(OrderAdded(LimitOrder(submitted, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))

    val exec = mkOrderExecutedRaw(submitted, counter)
    exec.executedAmount shouldBe 420169L

    oh.process(exec)
    withClue(s"counter.order.id=${submitted.idStr()}") {

      exec.counterRemainingAmount shouldBe 420171L
      exec.counterRemainingAmount shouldBe counter.amount - exec.executedAmount

      exec.counterRemainingFee shouldBe 150001L

      orderStatus(counter.id()) shouldBe OrderStatus.PartiallyFilled(exec.executedAmount, exec.counterExecutedFee)
    }

    withClue(s"submitted.order.id=${counter.idStr()}") {
      exec.submittedRemainingAmount shouldBe 5363L
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount

      exec.submittedRemainingFee shouldBe 3781L
      orderStatus(submitted.id()) shouldBe OrderStatus.Filled(exec.executedAmount, exec.submittedExecutedFee)
    }

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      val expected = Seq(counter.id())

      activeOrderIds(counter.senderPublicKey) shouldBe expected
      allOrderIds(counter.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe expected
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe Seq(submitted.id())
    }
  }

  property("Sell WAVES order - filled, buy order - partial") {
    val counter = LimitOrder(sell(WavesBtc, 100000000, 0.0008, matcherFee = Some(2000L)), None, None)
    val submitted = LimitOrder(buy(WavesBtc, 120000000, 0.00085, matcherFee = Some(1000L)), None, None)

    List(counter, submitted).foreach(o => oh.process(OrderAdded(o, OrderAddedReason.RequestExecuted, time.getTimestamp())))

    val exec = mkOrderExecuted(submitted, counter)
    oh.processAll(exec)

    withClue(s"counter: ${counter.order.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingFee shouldBe 0L

      orderStatus(counter.order.id()) shouldBe OrderStatus.Filled(100000000, 2000)
    }

    withClue(s"submitted: ${submitted.order.idStr()}") {
      exec.submittedRemainingAmount shouldBe 20000000L
      exec.submittedRemainingFee shouldBe 167L

      orderStatus(submitted.order.id()) shouldBe OrderStatus.PartiallyFilled(100000000, 833)
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.order.senderPublicKey}, submitted.order.id=${submitted.order.idStr()}") {
      openVolume(submitted.order.senderPublicKey, WavesBtc.priceAsset) shouldBe (BigDecimal(0.00085) * 20000000L).toLong
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.order.senderPublicKey) shouldBe empty
      allOrderIds(counter.order.senderPublicKey) shouldBe Seq(counter.order.id())

      activeOrderIdsByPair(counter.order.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(counter.order.senderPublicKey, WavesBtc) shouldBe Seq(counter.order.id())
    }

    withClue("orders list of submitted owner") {
      val expected = Seq(submitted.order.id())

      activeOrderIds(submitted.order.senderPublicKey) shouldBe expected
      allOrderIds(submitted.order.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(submitted.order.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(submitted.order.senderPublicKey, WavesBtc) shouldBe expected
    }
  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val counter = LimitOrder(buy(WavesBtc, 100000000, 0.0008, matcherFee = Some(300001L)), None, None)
    val submitted1 = LimitOrder(sell(WavesBtc, 50000000, 0.00075, matcherFee = Some(300001L)), None, None)
    val submitted2 = LimitOrder(sell(WavesBtc, 80000000, 0.0008, matcherFee = Some(300001L)), None, None)

    List(counter, submitted1).foreach(o => oh.process(OrderAdded(o, OrderAddedReason.RequestExecuted, time.getTimestamp())))
    val exec1 = mkOrderExecuted(submitted1, counter)
    oh.process(exec1)

    orderStatus(counter.order.id()) shouldBe OrderStatus.PartiallyFilled(50000000, 150000)
    orderStatus(submitted1.order.id()) shouldBe OrderStatus.Filled(50000000, 300001)

    val exec2 = mkOrderExecuted(submitted2, exec1.counterRemaining)
    oh.processAll(
      OrderAdded(submitted2, OrderAddedReason.RequestExecuted, time.getTimestamp()),
      exec2
    )

    withClue(s"counter: ${counter.order.idStr()}") {
      orderStatus(counter.order.id()) shouldBe OrderStatus.Filled(100000000, 300000)
    }

    orderStatus(submitted1.order.id()) shouldBe OrderStatus.Filled(50000000, 300001)
    orderStatus(submitted2.order.id()) shouldBe OrderStatus.PartiallyFilled(50000000, 187500)

    openVolume(counter.order.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    openVolume(counter.order.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    activeOrderIds(counter.order.senderPublicKey) shouldBe empty

    openVolume(submitted1.order.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    openVolume(submitted1.order.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    activeOrderIds(submitted1.order.senderPublicKey) shouldBe empty

    withClue(s"account checks, ord3.senderPublicKey: ${submitted2.order.senderPublicKey}, ord3.order.id=${submitted2.order.idStr()}") {
      openVolume(submitted2.order.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
      activeOrderIds(submitted2.order.senderPublicKey) shouldBe Seq(submitted2.order.id())
    }
  }

  property("WCT/BTC: sell - filled partially, buy - filled") {
    val pair = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
    val counter = sell(pair, 347, 0.12739213, matcherFee = Some(300000L))
    val submitted = buy(pair, 146, 0.12739213, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    val exec = mkOrderExecutedRaw(submitted, counter)
    oh.processAll(exec, OrderCanceled(exec.submittedRemaining, Events.OrderCanceledReason.BecameUnmatchable, time.getTimestamp()))

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 205L
      openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, Waves) shouldBe counter.matcherFee - AcceptedOrder.partialFee(
        counter.matcherFee,
        counter.amount,
        exec.executedAmount
      )
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Buy USD order - filled, sell BTC order - filled") {
    val pair = AssetPair(mkAssetId("USD"), mkAssetId("BTC"))
    val counter = buy(pair, 5000000, 0.001, matcherFee = Some(1000L))
    val submitted = sell(pair, 5000000, 0.00099908, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    val exec = mkOrderExecutedRaw(submitted, counter)
    oh.process(exec)

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Sell ETH twice (filled, partial), buy WAVES order - filled") {
    val pair = AssetPair(mkAssetId("ETH"), Waves)
    val counter1 = LimitOrder(sell(pair, 2864310, 0.003, matcherFee = Some(300000L)), None, None)
    val counter2 = LimitOrder(sell(pair, 7237977, 0.003, matcherFee = Some(300000L)), None, None)
    val submitted = LimitOrder(buy(pair, 4373667, 0.003, matcherFee = Some(300000L)), None, None)

    List(counter1, counter2).foreach(o => oh.process(OrderAdded(o, OrderAddedReason.RequestExecuted, time.getTimestamp())))

    val exec1 = mkOrderExecuted(submitted, counter1)
    oh.processAll(
      exec1,
      OrderCanceled(exec1.counterRemaining, Events.OrderCanceledReason.BecameUnmatchable, time.getTimestamp()),
      mkOrderExecuted(exec1.submittedLimitRemaining(submitted), counter2)
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.order.senderPublicKey}, submitted.order.id=${submitted.order.id()}") {
      openVolume(submitted.order.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.order.senderPublicKey, pair.priceAsset) shouldBe 0L
    }
  }

  property("Sell EUR - partial, buy EUR order - filled") {
    val pair = AssetPair(mkAssetId("EUR"), mkAssetId("USD"))
    val counter = sell(pair, 57918, 0.001356, matcherFee = Some(300000L))
    val submitted = buy(pair, 46978, 0.003333, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))

    val exec = mkOrderExecutedRaw(submitted, counter)
    oh.processAll(
      exec,
      OrderCanceled(exec.submittedRemaining, Events.OrderCanceledReason.BecameUnmatchable, time.getTimestamp()),
      OrderCanceled(exec.counterRemaining, Events.OrderCanceledReason.RequestExecuted, time.getTimestamp())
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Total execution of two counter orders and the one submitted") {
    val pair = AssetPair(mkAssetId("Alice"), Waves)

    val counter1 = LimitOrder(buy(pair, 150, 190000000L, matcherFee = Some(300000)), None, None)
    val counter2 = LimitOrder(buy(pair, 200, 200000000L, matcherFee = Some(300000)), None, None)
    val submitted = LimitOrder(sell(pair, 350, 210000000L, matcherFee = Some(300000)), None, None)

    List(counter1, counter2, submitted).foreach(o => oh.process(OrderAdded(o, OrderAddedReason.RequestExecuted, time.getTimestamp())))

    val exec1 = mkOrderExecuted(submitted, counter1)
    oh.processAll(
      exec1,
      mkOrderExecuted(exec1.submittedLimitRemaining(submitted), counter2)
    )

    orderStatus(submitted.order.id()) shouldBe OrderStatus.Filled(350, 299999)
  }

  property("Partially with own order") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val counter = LimitOrder(buy(WavesBtc, 100000000, 0.0008, Some(pk), Some(300000L)), None, None)
    val submitted = LimitOrder(sell(WavesBtc, 210000000, 0.00079, Some(pk), Some(300000L)), None, None)

    List(counter, submitted).foreach(o => oh.process(OrderAdded(o, OrderAddedReason.RequestExecuted, time.getTimestamp())))

    val exec = mkOrderExecuted(submitted, counter)
    oh.process(exec)

    withClue(s"counter: ${counter.order.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingFee shouldBe 0L
      orderStatus(counter.order.id()) shouldBe OrderStatus.Filled(100000000, 300000)
    }

    withClue(s"submitted: ${submitted.order.idStr()}") {
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount
    }

    openVolume(pk, WavesBtc.amountAsset) shouldBe 110157143L
    openVolume(pk, WavesBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      activeOrderIds(pk) shouldBe Seq(submitted.order.id())
      allOrderIds(pk) shouldBe Seq(submitted.order.id(), counter.order.id())

      activeOrderIdsByPair(pk, WavesBtc) shouldBe Seq(submitted.order.id())
      allOrderIdsByPair(pk, WavesBtc) shouldBe Seq(submitted.order.id(), counter.order.id())
    }
  }

  property("Cancel buy order") {
    val ord1 = buy(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))

    oh.processAll(
      OrderAdded(LimitOrder(ord1, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()),
      OrderCanceled(LimitOrder(ord1, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp())
    )

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0, 0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      val addr = ord1.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(ord1.id())

      activeOrderIdsByPair(addr, WctBtc) shouldBe empty
      allOrderIdsByPair(addr, WctBtc) shouldBe Seq(ord1.id())
    }
  }

  property("Cancel sell order") {
    val ord1 = sell(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(ord1, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    oh.process(OrderCanceled(LimitOrder(ord1, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp()))

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0, 0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey) shouldBe empty
  }

  property("Cancel partially executed order") {
    val counter = sell(WavesBtc, 2100000000, 0.0008, matcherFee = Some(300000L))
    val submitted = buy(WavesBtc, 1000000000, 0.00081, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
    val exec1 = mkOrderExecutedRaw(submitted, counter)
    oh.processAll(
      OrderAdded(LimitOrder(submitted, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()),
      exec1,
      OrderCanceled(
        exec1.counter.partial(exec1.counterRemainingAmount, exec1.counterRemainingFee, BigInteger.ZERO),
        Events.OrderCanceledReason.RequestExecuted,
        time.getTimestamp()
      )
    )

    eventually {
      orderStatus(counter.id()) shouldBe OrderStatus.Cancelled(1000000000, 142857)
    }
    orderStatus(submitted.id()) shouldBe OrderStatus.Filled(1000000000, 300000)

    openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L

    withClue("orders list of counter owner") {
      val addr = counter.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(counter.id())

      activeOrderIdsByPair(addr, WavesBtc) shouldBe empty
      allOrderIdsByPair(addr, WavesBtc) shouldBe Seq(counter.id())
    }

    activeOrderIds(submitted.senderPublicKey) shouldBe empty
  }

  property("Sorting by status then timestamp") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val ord1 = LimitOrder(buy(WavesBtc, 110000000, 0.0008, Some(pk), Some(300000L), Some(1L)), None, None) // Filled
    val ord2 = LimitOrder(buy(WavesBtc, 120000000, 0.0006, Some(pk), Some(300000L), Some(2L)), None, None) // Accepted
    val ord3 = LimitOrder(buy(WavesBtc, 130000000, 0.0005, Some(pk), Some(300000L), Some(3L)), None, None) // Canceled
    val ord4 = LimitOrder(sell(WavesBtc, 2100000000, 0.00079, Some(pk), Some(300000L), Some(4L)), None, None) // Partial
    val ord5 = LimitOrder(buy(WavesBtc, 130000000, 0.0004, Some(pk), Some(300000L), Some(45)), None, None) // Accepted

    List(ord1, ord2, ord3, ord4).foreach { ord =>
      oh.process(OrderAdded(ord, OrderAddedReason.RequestExecuted, time.getTimestamp()))
    }
    val exec = mkOrderExecuted(ord4, ord1)
    oh.processAll(
      exec,
      OrderCanceled(ord3, Events.OrderCanceledReason.RequestExecuted, time.getTimestamp()),
      OrderAdded(ord5, OrderAddedReason.RequestExecuted, time.getTimestamp())
    )

    allOrderIds(ord1.order.senderPublicKey) should matchTo(Vector(ord5, ord4, ord2, ord3, ord1).map(_.order.id()))
    activeOrderIds(ord1.order.senderPublicKey) should matchTo(Vector(ord5, ord4, ord2).map(_.order.id()))

    withClue("orders list") {
      val addr = pk.toAddress
      val allOrders = Seq(ord5, ord4, ord2, ord3, ord1).map(_.order.id())
      val activeOrders = Seq(ord5, ord4, ord2).map(_.order.id())

      activeOrderIds(addr) shouldBe activeOrders
      allOrderIds(addr) shouldBe allOrders

      activeOrderIdsByPair(addr, WavesBtc) shouldBe activeOrders
      allOrderIdsByPair(addr, WavesBtc) shouldBe allOrders
    }
  }

  property("History with more than max limit") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val origOrders = (0 until matcherSettings.orderDb.maxOrders).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    val canceledOrder = origOrders.last
    oh.process(OrderCanceled(LimitOrder(canceledOrder, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp()))

    val newOrder = buy(WavesBtc, 100000000, 0.001, Some(pk), Some(300000L), Some(1L))
    oh.process(OrderAdded(LimitOrder(newOrder, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))

    withClue("orders list") {
      // 'last' is canceled, remove it
      val expectedActiveOrders = origOrders.init.reverse :+ newOrder
      activeOrderIds(pk) shouldBe expectedActiveOrders.map(_.id())

      // 'last' is canceled. It should be moved to the end of all orders' list
      val expectedAllOrders = origOrders.init.reverse :+ newOrder :+ canceledOrder
      val actualAllOrders = allOrderIds(pk)
      actualAllOrders should matchTo(expectedAllOrders.map(_.id()))
    }
  }

  property("History with canceled order and more than max limit") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val origOrders = (0 to matcherSettings.orderDb.maxOrders).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    val canceledOrder = origOrders.last
    oh.process(OrderCanceled(LimitOrder(canceledOrder, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp()))

    withClue("orders list") {
      // 'last' is canceled, remove it
      val expectedActiveOrders = origOrders.init.reverse
      activeOrderIds(pk) shouldBe expectedActiveOrders.map(_.id())

      val expectedAllOrders = expectedActiveOrders :+ canceledOrder
      val actualAllOrders = allOrderIds(pk)
      actualAllOrders should matchTo(expectedAllOrders.map(_.id()))
    }
  }

  property("History by pair - added orders more than history by pair limit (200 active)") {
    val pk = KeyPair("private".getBytes("utf-8"))

    val orders = (1 to MaxActiveOrders).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    val expectedIds = orders.map(_.id()).reverse

    withClue("common") {
      val allIds = allOrderIds(pk)
      allIds shouldBe expectedIds
      activeOrderIds(pk) shouldBe expectedIds
    }

    withClue("pair") {
      // Even expectedIds.size < pair.MaxElements!
      allOrderIdsByPair(pk, WavesBtc) shouldBe expectedIds
      activeOrderIdsByPair(pk, WavesBtc) shouldBe expectedIds
    }
  }

  property("History by pair - added and canceled orders both more than history by pair limit (maxActive + maxFinalized + 1)") {
    val pk = KeyPair("private".getBytes("utf-8"))

    val ordersToFinalize = MaxFinalizedOrders + 1
    val allOrders = (1 to MaxActiveOrders + ordersToFinalize).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.processAll(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    val (ordersToCancel, activeOrders) = allOrders.splitAt(ordersToFinalize)
    ordersToCancel.foreach(o =>
      oh.process(OrderCanceled(LimitOrder(o, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp()))
    )
    val expectedActiveOrderIds = activeOrders.map(_.id()).reverse

    withClue("common") {
      val expectedAllIds = allOrders.takeRight(MaxTotalOrders).map(_.id()).reverse
      val allIds = allOrderIds(pk)
      allIds should matchTo(expectedAllIds)
      activeOrderIds(pk) should matchTo(expectedActiveOrderIds)
    }

    withClue("pair") {
      val expectedIds = allOrders.takeRight(MaxTotalOrders).map(_.id()).reverse
      val pair1Ids = allOrderIdsByPair(pk, WavesBtc)
      pair1Ids should matchTo(expectedIds)
      activeOrderIdsByPair(pk, WavesBtc) should matchTo(expectedActiveOrderIds)
    }
  }

  property("History by pair contains more elements than in common") {
    val pk = KeyPair("private".getBytes("utf-8"))
    val pair1 = WavesBtc
    val pair2 = AssetPair(Waves, mkAssetId("ETH"))

    // 1. Place and cancel MaxTotalOrders orders

    val pair1Orders = (1 to MaxTotalOrders).map { i =>
      val o = buy(pair1, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    pair1Orders.foreach(o => oh.process(OrderCanceled(LimitOrder(o, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp())))

    withClue("after 1 step") {
      activeOrderIds(pk) shouldBe empty
      activeOrderIdsByPair(pk, pair1) shouldBe empty
      activeOrderIdsByPair(pk, pair2) shouldBe empty

      val expectedIds = pair1Orders.map(_.id()).reverse

      withClue("common") {
        allOrderIds(pk) should matchTo(expectedIds.take(MaxFinalizedOrders))
      }

      withClue("pair1") {
        allOrderIdsByPair(pk, pair1) should matchTo(expectedIds.take(MaxFinalizedOrders))
      }

      withClue("pair2") {
        val pair2Ids = allOrderIdsByPair(pk, pair2)
        pair2Ids shouldBe empty
      }
    }

    // 2. Place and cancel 10 orders in pair2

    val pair2Orders = (1 to 10).map { i =>
      val o = buy(pair2, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(1000L + i))
      oh.process(OrderAdded(LimitOrder(o, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()))
      o
    }.toVector

    pair2Orders.foreach(o => oh.process(OrderCanceled(LimitOrder(o, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp())))

    withClue("after 2 step") {
      activeOrderIds(pk) shouldBe empty
      activeOrderIdsByPair(pk, pair1) shouldBe empty
      activeOrderIdsByPair(pk, pair2) shouldBe empty

      withClue("common") {
        val allIds = allOrderIds(pk)
        val expectedIds = (pair1Orders.map(_.id()) ++ pair2Orders.map(_.id())).reverse.take(MaxFinalizedOrders)
        allIds should matchTo(expectedIds)
      }

      withClue("pair1") {
        val pair1Ids = allOrderIdsByPair(pk, pair1)
        pair1Ids should matchTo(pair1Orders.map(_.id()).reverse.take(MaxFinalizedOrders))
      }

      withClue("pair2") {
        val pair2Ids = allOrderIdsByPair(pk, pair2)
        pair2Ids should matchTo(pair2Orders.map(_.id()).reverse.take(MaxFinalizedOrders))
      }
    }
  }

  property("Wrong events - OrderExecuted for non-existed orders") {
    val counter = buy(WavesBtc, 100000, 0.0008, matcherFee = Some(2000L))
    val submitted = sell(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(mkOrderExecutedRaw(submitted, counter))

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
    }
  }

  property("Idempotence - OrderAdded") {
    val ord = buy(WctBtc, 10000, 0.0007)

    val lo = LimitOrder(ord, None, None)
    val add = OrderAdded(lo, OrderAddedReason.RequestExecuted, time.getTimestamp())
    oh.processAll(add, add)

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 7L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("Idempotence - OrderCancelled") {
    val ord1 = buy(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))
    val cancel = OrderCanceled(LimitOrder(ord1, None, None), Events.OrderCanceledReason.RequestExecuted, time.getTimestamp())
    oh.processAll(OrderAdded(LimitOrder(ord1, None, None), OrderAddedReason.RequestExecuted, time.getTimestamp()), cancel, cancel)

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0, 0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      val addr = ord1.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(ord1.id())

      activeOrderIdsByPair(addr, WctBtc) shouldBe empty
      allOrderIdsByPair(addr, WctBtc) shouldBe Seq(ord1.id())
    }
  }
}

private object OrderHistoryBalanceSpecification {

  val MaxActiveOrders = 100
  val MaxFinalizedOrders = 70
  val MaxTotalOrders = MaxActiveOrders + MaxFinalizedOrders

  implicit val askTimeout: Timeout = 5.seconds

  private def askAddressActor[A: ClassTag](ref: ActorRef, msg: Any)(implicit pc: PatienceConfig, pos: source.Position) =
    (ref ? msg).mapTo[A].futureValue(pc, pos)

  implicit private class AddressActorExt(val ref: ActorRef) extends AnyVal {

    def orderIds(assetPair: Option[AssetPair], orderListType: OrderListType): Vector[Order.Id] =
      askAddressActor[AddressActor.Reply.GetOrderStatuses](ref, AddressActor.Query.GetOrdersStatuses(assetPair, orderListType))
        .xs
        .map(_._1).toVector

    def activeOrderIds: Vector[Order.Id] = orderIds(None, OrderListType.ActiveOnly)

    def allOrderIds: Vector[Order.Id] = orderIds(None, OrderListType.All)

    def activeOrderIdsByPair(pair: AssetPair): Vector[Order.Id] = orderIds(Some(pair), OrderListType.ActiveOnly)

    def allOrderIdsByPair(pair: AssetPair): Vector[Order.Id] = orderIds(Some(pair), OrderListType.All)

    def openVolume(asset: Asset): Long =
      askAddressActor[AddressActor.Reply.GetBalance](ref, AddressActor.Query.GetReservedBalance).balance.getOrElse(asset, 0L)

    def orderStatus(orderId: ByteStr): OrderStatus =
      askAddressActor[AddressActor.Reply.GetOrderStatus](ref, AddressActor.Query.GetOrderStatus(orderId)).x

  }

}
