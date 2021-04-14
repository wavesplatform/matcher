package com.wavesplatform.dex.model.address

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import cats.syntax.either._
import com.wavesplatform.dex.actors.address.AddressActor.Query
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.db.TestOrderDb
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.model.address.AddressActorStartingBenchmark.AddressState
import com.wavesplatform.dex.model.{AcceptedOrder, Events, LimitOrder}
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import com.wavesplatform.dex.time.TestTime
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen

import java.nio.charset.StandardCharsets
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class AddressActorStartingBenchmark {
  @Benchmark def eventsProcessingDuringStartTest(st: AddressState, bh: Blackhole): Unit = bh.consume(st.run())
}

object AddressActorStartingBenchmark {

  @State(Scope.Thread) class AddressState extends OrderBookGen {

    val maxPrice: Long = 1000L * Order.PriceConstant
    val minPrice: Long = 1L * Order.PriceConstant
    val priceGen: Gen[Long] = Gen.chooseNum(minPrice, maxPrice)

    val owner: KeyPair = clientGen.sample.get
    val counterOrderOwner: KeyPair = KeyPair(ByteStr("counter".getBytes(StandardCharsets.UTF_8)))

    val askGen: Gen[Order] = orderGen(owner, priceGen, OrderType.SELL)
    val bidGen: Gen[Order] = orderGen(owner, priceGen, OrderType.BUY)
    val orderGen: Gen[Order] = Gen.oneOf(askGen, bidGen)
    val limitOrderGen: Gen[LimitOrder] = orderGen.map(LimitOrder(_))

    val orderAddedGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      Seq(getOrderAddedEvent(ao))
    }

    val orderCanceledGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      Seq(getOrderAddedEvent(ao), getOrderCancelledEvent(ao))
    }

    val orderFilledImmediatelyGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      Seq(getOrderAddedEvent(ao), getOrderExecutedEvent(ao, getCounter(ao, ao.amount)))
    }

    val orderFilledEventuallyGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      val firstExecutedEvent = getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2))
      val secondExecutedEvent = getOrderExecutedEvent(firstExecutedEvent.submittedRemaining, getCounter(ao, ao.amount))
      Seq(getOrderAddedEvent(ao), firstExecutedEvent, secondExecutedEvent)
    }

    val orderPartiallyFilledGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      Seq(getOrderAddedEvent(ao), getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2)))
    }

    val orderPartiallyFilledAndThenCancelledGen: Gen[Seq[Events.Event]] = limitOrderGen.map { ao =>
      val firstExecutedEvent = getOrderExecutedEvent(ao, getCounter(ao, ao.amount / 2))
      val cancelEvent = getOrderCancelledEvent(firstExecutedEvent.submittedRemaining)
      Seq(getOrderAddedEvent(ao), firstExecutedEvent, cancelEvent)
    }

    val eventsGen = Gen.frequency(
      150 -> orderAddedGen,
      100 -> orderCanceledGen,
      200 -> orderFilledImmediatelyGen,
      200 -> orderFilledEventuallyGen,
      250 -> orderPartiallyFilledGen,
      100 -> orderPartiallyFilledAndThenCancelledGen
    )

    implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

    def run(): AddressActor.Reply.GetOrderStatuses = {

      val system: ActorSystem = ActorSystem(s"addressActorBenchmark-${ThreadLocalRandom.current().nextInt()}")

      val addressActor: ActorRef =
        system.actorOf(
          AddressActor.props(
            owner = owner,
            time = new TestTime(),
            orderDb = TestOrderDb(10000),
            validate = (_, _) => Future.successful(().asRight),
            store = command => Future.successful(Some(ValidatedCommandWithMeta(0L, 0L, command))),
            recovered = true,
            blockchain = (_: Address, _: Set[Asset]) => Future.successful(AddressBalanceUpdates.empty),
            settings = AddressActor.Settings.default
          )
        )

      eventsGen.sample.get.foreach(addressActor ! _)

      addressActor ! AddressDirectoryActor.Command.StartWork

      val ordersStatuses = Await.result(
        addressActor.ask(Query.GetOrdersStatuses(None, AddressActor.OrderListType.All))(3.minutes).mapTo[AddressActor.Reply.GetOrderStatuses],
        3.minutes
      )

      Await.result(system.terminate(), 3.seconds)
      ordersStatuses
    }

    def ordersGen(orderNumber: Int): Gen[List[AcceptedOrder]] =
      for {
        orderSides <- Gen.listOfN(orderNumber, orderSideGen)
        orders <- Gen.sequence[List[LimitOrder], LimitOrder] {
          orderSides.map { side =>
            (if (side == OrderType.SELL) askGen else bidGen).map(LimitOrder.apply)
          }
        }
      } yield orders

    def getOrderAddedEvent(ao: AcceptedOrder): Events.OrderAdded =
      Events.OrderAdded(ao, Events.OrderAddedReason.RequestExecuted, System.currentTimeMillis())

    def getOrderCancelledEvent(ao: AcceptedOrder): Events.OrderCanceled =
      Events.OrderCanceled(ao, Events.OrderCanceledReason.RequestExecuted, System.currentTimeMillis())

    def getCounter(ao: AcceptedOrder, amount: Long): LimitOrder = LimitOrder(ao.order.updateAmount(amount).updateSender(counterOrderOwner))

    def getOrderExecutedEvent(ao: AcceptedOrder, counter: LimitOrder): Events.OrderExecuted = Events.OrderExecuted(
      ao,
      counter,
      System.currentTimeMillis(),
      counter.matcherFee,
      ao.matcherFee
    )

  }

}
