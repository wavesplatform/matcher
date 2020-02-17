package com.wavesplatform.dex.model

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.OrderBookBenchmark._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen

import scala.collection.JavaConverters._
import scala.util.Random

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class OrderBookBenchmark {
//  @Benchmark
//  def add_test(st: AddState, bh: Blackhole): Unit = {
//    val ob = OrderBook.empty()
//    val ts = 1000
//    bh.consume {
//      st.orders.map { o =>
//        ob.add(o, ts, getMakerTakerFee = (a, b) => (a.matcherFee, b.matcherFee), tickSize = 10)
//      }
//    }
//  }

  @Benchmark
  def cancel_test(st: CancelState, bh: Blackhole): Unit = {
    val ts = 1000
    bh.consume {
      st.orders.map(st.orderBook.cancel(_, ts))
    }
  }
}

object OrderBookBenchmark {

  @State(Scope.Benchmark)
  class AddState extends OrderBookGen {
    private val orderNumber = 1000

    private val maxPrice = 1000L * Order.PriceConstant
    private val minPrice = 1L * Order.PriceConstant
    private val priceGen = Gen.chooseNum(minPrice, maxPrice)

    private val askGen = orderGen(priceGen, OrderType.SELL)
    private val bidGen = orderGen(priceGen, OrderType.BUY)

    private val ordersGen: Gen[List[AcceptedOrder]] = for {
      orderSides <- Gen.listOfN(orderNumber, orderSideGen)
      orders <- Gen.sequence {
        orderSides.map { side =>
          val orderGen = if (side == OrderType.SELL) askGen else bidGen
          Gen.oneOf(limitOrderGen(orderGen), marketOrderGen(orderGen))
        }
      }
    } yield orders.asScala.toList

    val orders: List[AcceptedOrder] = ordersGen.sample.get
  }

  @State(Scope.Benchmark)
  class CancelState extends OrderBookGen {
    private val orderNumber = 1000

    private val askPricesMin = 1000L * Order.PriceConstant
    private val askPricesMax = 2000L * Order.PriceConstant
    private val askPricesGen = Gen.choose(askPricesMin, askPricesMax)

    private val bidPricesMin = 1L * Order.PriceConstant
    private val bidPricesMax = 999L * Order.PriceConstant
    private val bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)

    val orderBookGen: Gen[OrderBook] = for {
      (asksLevels, askOrders) <- sideOrdersGen(OrderType.SELL, maxLevelsInOrderBook, askPricesGen)
      (_, bidOrders)          <- sideOrdersGen(OrderType.BUY, maxLevelsInOrderBook - asksLevels, bidPricesGen)
    } yield mkOrderBook(askOrders, bidOrders)

    val orderBook: OrderBook = orderBookGen.sample.get
    val orders: Seq[Order.Id] = {
      val r = orderBook.allOrders.map(_._2.order.id()).toVector
      new Random(ThreadLocalRandom.current()).shuffle(r)
    }
  }

}
