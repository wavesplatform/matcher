package com.wavesplatform.dex.model.address

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import cats.syntax.either._
import com.wavesplatform.dex.actors.address.AddressActor.Query
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.db.TestOrderDB
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.address.AddressActorStartingBenchmark.AddressState
import com.wavesplatform.dex.model.{AcceptedOrder, Events, LimitOrder}
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.dex.time.TestTime
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.util.Random

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class AddressActorStartingBenchmark {
  @Benchmark def eventsProcessingDuringStartTest(st: AddressState, bh: Blackhole): Unit = bh.consume { st.run() }
}

object AddressActorStartingBenchmark {

  object OrderEvent extends Enumeration {
    type OrderEvent = Value
    val Added, Cancelled, FilledImmediately, FilledEventually, PartiallyFilled, PartiallyFilledAndThenCancelled = Value
  }

  @State(Scope.Thread) class AddressState extends OrderBookGen {

    val orderAddedEventCount: Int                      = 150
    val orderCancelledEventCount: Int                  = 100
    val orderFilledImmediatelyCount: Int               = 200
    val orderFilledEventuallyCount: Int                = 200
    val orderPartiallyFilledCount: Int                 = 250
    val orderPartiallyFilledAndThenCancelledCount: Int = 100

    val ordersCount: Int =
      orderAddedEventCount +
        orderCancelledEventCount +
        orderFilledImmediatelyCount +
        orderFilledEventuallyCount +
        orderPartiallyFilledCount +
        orderPartiallyFilledAndThenCancelledCount

    val maxPrice: Long      = 1000L * Order.PriceConstant
    val minPrice: Long      = 1L * Order.PriceConstant
    val priceGen: Gen[Long] = Gen.chooseNum(minPrice, maxPrice)

    val owner: KeyPair   = clientGen.sample.get
    val counter: KeyPair = clientGen.sample.get

    val askGen: Gen[Order] = orderGen(owner, priceGen, OrderType.SELL)
    val bidGen: Gen[Order] = orderGen(owner, priceGen, OrderType.BUY)

    val orders: List[AcceptedOrder] = ordersGen(ordersCount).sample.get

    val (eventsGroups, _) =
      Seq(
        orderAddedEventCount                      -> OrderEvent.Added,
        orderCancelledEventCount                  -> OrderEvent.Cancelled,
        orderFilledImmediatelyCount               -> OrderEvent.FilledImmediately,
        orderFilledEventuallyCount                -> OrderEvent.FilledEventually,
        orderPartiallyFilledCount                 -> OrderEvent.PartiallyFilled,
        orderPartiallyFilledAndThenCancelledCount -> OrderEvent.PartiallyFilledAndThenCancelled
      ).foldLeft((Seq.empty[Seq[Events.Event]], orders)) { case ((result, ordersRemaining), (groupSize, event)) =>
        val (ordersGroup, newRemaining) = ordersRemaining.splitAt(groupSize)
        val eventsGroup =
          event match {
            case OrderEvent.Added     => ordersGroup.map(getOrderAddedEvent)
            case OrderEvent.Cancelled => ordersGroup.flatMap { ao => Seq(getOrderAddedEvent(ao), getOrderCancelledEvent(ao)) }
            case OrderEvent.FilledImmediately =>
              ordersGroup.flatMap { ao => Seq(getOrderAddedEvent(ao), getOrderExecutedEvent(ao, getCounter(ao, ao.amount))) }
            case OrderEvent.FilledEventually =>
              ordersGroup.flatMap { ao =>
                val firstExecutedEvent  = getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2))
                val secondExecutedEvent = getOrderExecutedEvent(firstExecutedEvent.submittedRemaining, getCounter(ao, ao.amount))
                Seq(getOrderAddedEvent(ao), firstExecutedEvent, secondExecutedEvent)
              }
            case OrderEvent.PartiallyFilled =>
              ordersGroup.flatMap { ao => Seq(getOrderAddedEvent(ao), getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2))) }
            case OrderEvent.PartiallyFilledAndThenCancelled =>
              ordersGroup.flatMap { ao =>
                val firstExecutedEvent = getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2))
                val cancelEvent        = getOrderCancelledEvent(firstExecutedEvent.submittedRemaining)
                Seq(getOrderAddedEvent(ao), firstExecutedEvent, cancelEvent)
              }
          }
        (result :+ eventsGroup) -> newRemaining
      }

    implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

    def run(): AddressActor.Reply.OrdersStatuses = {

      val system: ActorSystem = ActorSystem("addressActorBenchmark")

      val addressActor: ActorRef =
        system.actorOf(
          AddressActor.props(
            owner = owner,
            time = new TestTime(),
            orderDB = new TestOrderDB(1000),
            validate = (_, _) => Future.successful(().asRight),
            store = event => Future.successful { Some(QueueEventWithMeta(0L, 0L, event)) },
            started = true,
            spendableBalancesActor = ActorRef.noSender,
            settings = AddressActor.Settings.default
          )
        )

      for {
        events <- Random.shuffle(eventsGroups)
        event  <- events
      } addressActor ! event

      addressActor ! AddressDirectoryActor.StartWork

      val ordersStatuses = Await.result(
        addressActor.ask(Query.GetOrdersStatuses(None, AddressActor.OrderListType.All))(3.minutes).mapTo[AddressActor.Reply.OrdersStatuses],
        3.minutes
      )

      Await.result(system.terminate(), 3.seconds)
      ordersStatuses
    }

    def ordersGen(orderNumber: Int): Gen[List[AcceptedOrder]] =
      for {
        orderSides <- Gen.listOfN(orderNumber, orderSideGen)
        orders <- Gen.sequence {
          orderSides.map { side =>
            (if (side == OrderType.SELL) askGen else bidGen).map(LimitOrder.apply)
          }
        }
      } yield orders.asScala.toList

    def getOrderAddedEvent(ao: AcceptedOrder): Events.OrderAdded =
      Events.OrderAdded(ao, Events.OrderAddedReason.RequestExecuted, System.currentTimeMillis())

    def getOrderCancelledEvent(ao: AcceptedOrder): Events.OrderCanceled =
      Events.OrderCanceled(ao, Events.OrderCanceledReason.RequestExecuted, System.currentTimeMillis())

    def getCounter(ao: AcceptedOrder, amount: Long): LimitOrder = {
      LimitOrder(orderGen(counter, priceGen, ao.order.orderType.opposite).map(_.updateAmount(amount).updatePrice(ao.price)).sample.get)
    }

    def getOrderExecutedEvent(ao: AcceptedOrder, counter: LimitOrder): Events.OrderExecuted = {
      Events.OrderExecuted(
        ao,
        counter,
        System.currentTimeMillis(),
        counter.matcherFee,
        ao.matcherFee
      )
    }
  }
}
