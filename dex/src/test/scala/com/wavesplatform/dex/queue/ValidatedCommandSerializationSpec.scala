package com.wavesplatform.dex.queue

import com.softwaremill.diffx.{Derived, Diff, Identical}
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.MatcherSpec
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{OrderType, OrderV1}
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder}
import com.wavesplatform.dex.queue.ValidatedCommand.{CancelOrder, DeleteOrderBook, PlaceMarketOrder, PlaceOrder}
import kamon.Kamon
import kamon.context.Context
import kamon.tag.{Tag, TagSet}
import kamon.trace.Span

import scala.concurrent.duration._

final class ValidatedCommandSerializationSpec extends MatcherSpec with MatcherSpecBase {

  "ValidatedCommandSerializationSpec" should {

    "write & read cancel order (1)" in test {
      CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, Some(order.sender.toAddress), Some(Kamon.currentContext()))
    }

    "write & read cancel order (2)" in test {
      CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, None, Some(Kamon.currentContext()))
    }

    "write & read cancel order (3)" in test {
      CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, Some(order.sender.toAddress), None)
    }

    "write & read cancel order (4)" in test {
      CancelOrder(order.assetPair, order.id(), Source.BalanceTracking, None, None)
    }

    "write & read place order (1)" in test {
      PlaceOrder(LimitOrder(order), Some(Kamon.currentContext()))
    }

    "write & read place order (2)" in test {
      PlaceOrder(LimitOrder(order), None)
    }

    "write & read place market order (1)" in test {
      PlaceMarketOrder(MarketOrder(order, 123L), Some(Kamon.currentContext()))
    }

    "write & read place market order (2)" in test {
      PlaceMarketOrder(MarketOrder(order, 123L), None)
    }

    "write & read delete order book (1)" in test {
      DeleteOrderBook(order.assetPair, Some(Kamon.currentContext()))
    }

    "write & read delete order book (2)" in test {
      DeleteOrderBook(order.assetPair, None)
    }

  }

  private def test(cmd: => ValidatedCommand): Unit = {
    def testInternal(): Unit = {
      val parsedCmd = ValidatedCommand.fromBytes(ValidatedCommand.toBytes(cmd))
      cmd should matchTo(parsedCmd)
    }

    //test with empty ctx
    testInternal()

    //test with non empty ctx
    val ctx = Context.of(TagSet.from(Map("tag1" -> "123", "tag2" -> "asdqew")))
    Kamon.runWithContext(ctx) {
      Kamon.runWithSpan(Kamon.spanBuilder("test-operation").start()) {
        testInternal()
      }
    }
  }

  private lazy val WUSD = IssuedAsset(ByteStr.decodeBase58("HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj").get)

  private lazy val publicKey: PublicKey = PublicKey.fromBase58String("5F3V3tYL7dqoMTwPFbC8BmACM3hDcaYoPwDaQpgTEDhw").toOption.get

  private lazy val order = OrderV1(
    sender = privateKey("test"),
    matcher = publicKey,
    pair = AssetPair(Waves, WUSD),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = System.currentTimeMillis(),
    expiration = System.currentTimeMillis() + 5.days.toMillis,
    matcherFee = matcherFee
  )

  implicit override protected def placeOrderDiff: Derived[Diff[PlaceOrder]] =
    Derived(Diff.gen[PlaceOrder])

  implicit override protected def placeMarketOrderDiff: Derived[Diff[PlaceMarketOrder]] =
    Derived(Diff.gen[PlaceMarketOrder])

  implicit override protected def cancelOrderDiff: Derived[Diff[CancelOrder]] =
    Derived(Diff.gen[CancelOrder])

  implicit override protected def deleteOrderBookDiff: Derived[Diff[DeleteOrderBook]] =
    Derived(Diff.gen[DeleteOrderBook])

  implicit def ctxDiff: Derived[Diff[Option[Context]]] = Derived {
    (maybeLeft: Option[Context], maybeRight: Option[Context], _: List[_root_.com.softwaremill.diffx.FieldPath]) =>
      (maybeLeft, maybeRight) match {
        case (Some(Context.Empty), None) => Identical(maybeLeft)
        case (None, Some(Context.Empty)) => Identical(maybeRight)
        case (maybeLeft, maybeRight) =>
          Diff.diffForOption[Context] {
            (left: Context, right: Context, _: List[_root_.com.softwaremill.diffx.FieldPath]) =>
              val filterKeys = Seq("upstream.name")
              val leftSpan = left.get(Span.Key)
              val leftMap = left.tags.all().map {
                case x: Tag.Boolean => x.key -> x.value.toString
                case x: Tag.String => x.key -> x.value
                case x: Tag.Long => x.key -> x.value.toString
              }.toMap.filterNot { case (k, _) => filterKeys.contains(k) }.map { case (k, v) => "_" + k -> v } ++
                Map("traceId" -> leftSpan.trace.id.string, "spanId" -> leftSpan.id.string)
              val rightSpan = right.get(Span.Key)
              val rightMap = right.tags.all().map {
                case x: Tag.Boolean => x.key -> x.value.toString
                case x: Tag.String => x.key -> x.value
                case x: Tag.Long => x.key -> x.value.toString
              }.toMap.filterNot { case (k, _) => filterKeys.contains(k) }.map { case (k, v) => "_" + k -> v } ++
                Map("traceId" -> rightSpan.trace.id.string, "spanId" -> rightSpan.id.string)

              Diff.diffForMap[String, String, Map].apply(leftMap, rightMap)
          }(maybeLeft, maybeRight)
      }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Kamon.init()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    Kamon.stop()
  }

}
