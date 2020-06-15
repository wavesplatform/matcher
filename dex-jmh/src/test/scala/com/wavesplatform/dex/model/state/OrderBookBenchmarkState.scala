package com.wavesplatform.dex.model.state

import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder}
import org.scalacheck.Gen

import scala.jdk.CollectionConverters._

trait OrderBookBenchmarkState extends OrderBookGen {
  def getMakerTakerFee(a: AcceptedOrder, b: LimitOrder): (Long, Long) = (a.matcherFee, b.matcherFee)

  def fixedSidesOrdersGen(levelNumber: Int,
                          orderNumberInLevel: Int,
                          askPricesGen: Gen[Long],
                          bidPricesGen: Gen[Long]): Gen[(Seq[LimitOrder], Seq[LimitOrder])] =
    for {
      askOrders <- fixedSideOrdersGen(OrderType.SELL, levelNumber / 2, orderNumberInLevel, askPricesGen)
      bidOrders <- fixedSideOrdersGen(OrderType.BUY, levelNumber / 2, orderNumberInLevel, bidPricesGen)
    } yield (askOrders, bidOrders)

  def fixedSideOrdersGen(side: OrderType, levels: Int, ordersInLevel: Int, pricesGen: Gen[Long]): Gen[Seq[LimitOrder]] =
    for {
      prices <- Gen.listOfN(levels, pricesGen)
      orders <- Gen.sequence(prices.map { price =>
        Gen.listOfN(ordersInLevel, limitOrderGen(orderGen(Gen.const(price), side)))
      })
    } yield orders.asScala.flatten.toSeq
}
