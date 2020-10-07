package com.wavesplatform.dex.model.orderbook

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.orderbook.OrderBookAddBenchmark._
import com.wavesplatform.dex.model.state.OrderBookBenchmarkState
import com.wavesplatform.dex.model.{AcceptedOrder, OrderBook}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen

import scala.jdk.CollectionConverters._

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class OrderBookAddBenchmark {
  @Benchmark def add_0_plus_1250_test(st: Add_0_To_1250_State, bh: Blackhole): Unit = bh.consume(st.run())
  @Benchmark def add_1250_plus_1250_test(st: Add_1250_To_1250_State, bh: Blackhole): Unit = bh.consume(st.run())
}

object OrderBookAddBenchmark {

  @State(Scope.Thread) class Add_0_To_1250_State extends AddState(initOrderNumber = 0, orderNumberToAdd = 1250)
  @State(Scope.Thread) class Add_1250_To_1250_State extends AddState(initOrderNumber = 1250, orderNumberToAdd = 1250)

  sealed abstract class AddState(initOrderNumber: Int, orderNumberToAdd: Int) extends OrderBookBenchmarkState {

    val maxPrice = 1000L * Order.PriceConstant
    val minPrice = 1L * Order.PriceConstant
    val priceGen = Gen.chooseNum(minPrice, maxPrice)

    val askGen = orderGen(priceGen, OrderType.SELL)
    val bidGen = orderGen(priceGen, OrderType.BUY)

    val orderBook: OrderBook = ordersGen(initOrderNumber).sample.get.foldLeft(OrderBook.empty)(_.add(_, ts, getMakerTakerFee).orderBook)

    val orders: List[AcceptedOrder] = ordersGen(orderNumberToAdd).sample.get

    def run(): OrderBook = orders.foldLeft(OrderBook.empty) { case (r, o) =>
      r.add(o, ts, getMakerTakerFee).orderBook
    }

    def ordersGen(orderNumber: Int): Gen[List[AcceptedOrder]] =
      for {
        orderSides <- Gen.listOfN(orderNumber, orderSideGen)
        orders <- Gen.sequence {
          orderSides.map { side =>
            val orderGen = if (side == OrderType.SELL) askGen else bidGen
            Gen.oneOf(limitOrderGen(orderGen), marketOrderGen(orderGen))
          }
        }
      } yield orders.asScala.toList

  }

}
