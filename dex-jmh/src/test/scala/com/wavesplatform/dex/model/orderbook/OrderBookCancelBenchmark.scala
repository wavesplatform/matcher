package com.wavesplatform.dex.model.orderbook

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.orderbook.OrderBookCancelBenchmark._
import com.wavesplatform.dex.model.state.OrderBookBenchmarkState
import com.wavesplatform.dex.model.{Events, OrderBook}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen

import scala.util.Random

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class OrderBookCancelBenchmark {
//  @Benchmark def cancel_2500_to_1250_test(st: Cancel_2500_To_1250_State, bh: Blackhole): Unit = bh.consume { st.run() }
  @Benchmark def cancel_1250_to_0_test(st: Cancel_1250_To_0_State, bh: Blackhole): Unit = bh.consume(st.run())
}

object OrderBookCancelBenchmark {

  @State(Scope.Thread) class Cancel_2500_To_1250_State extends CancelState(initOrderNumber = 2500, orderNumberAfterCancel = 1250)
  @State(Scope.Thread) class Cancel_1250_To_0_State extends CancelState(initOrderNumber = 1250, orderNumberAfterCancel = 0)

  sealed abstract class CancelState(initOrderNumber: Int, orderNumberAfterCancel: Int) extends OrderBookBenchmarkState {
    private val askPricesMin = 1000L * Order.PriceConstant
    private val askPricesMax = 2000L * Order.PriceConstant

    private val bidPricesMin = 1L * Order.PriceConstant
    private val bidPricesMax = 999L * Order.PriceConstant

    val orderBookGen: Gen[OrderBook] = fixedSidesOrdersGen(
      levelNumber = initOrderNumber / 2,
      orderNumberInLevel = 2,
      askPricesGen = Gen.choose(askPricesMin, askPricesMax),
      bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)
    ).map(Function.tupled(mkOrderBook))

    val orderBook: OrderBook = orderBookGen.sample.get

    val orders: Seq[Order.Id] = {
      val xs = orderBook.allOrders.map(_.order.id()).toVector
      new Random(ThreadLocalRandom.current()).shuffle(xs).take(initOrderNumber - orderNumberAfterCancel)
    }

    def run(): OrderBook = orders.foldLeft(orderBook) { case (r, id) => r.cancel(id, Events.OrderCanceledReason.RequestExecuted, ts)._1 }
  }

}
