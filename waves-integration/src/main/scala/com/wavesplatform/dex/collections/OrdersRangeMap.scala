package com.wavesplatform.dex.collections

import com.wavesplatform.dex.collections.OrdersRangeMap._
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order

import scala.collection.immutable.{HashMap, TreeMap}

trait OrdersRangeMap {

  def contains(id: OrderId): Boolean

  def get(id: OrderId): Option[Order]
  def range(price: StopPrice): Iterable[Order]
  def range(begin: StopPrice, end: StopPrice): Iterable[Order]

  def updated(price: StopPrice, order: Order): OrdersRangeMap

  def removed(id: OrderId): OrdersRangeMap
  def removedAll(price: StopPrice): OrdersRangeMap
  def removedAll(begin: StopPrice, end: StopPrice): OrdersRangeMap

}

object OrdersRangeMap {

  type StopPrice = Long
  type OrderId = ByteStr

  private[this] type Number = Long

  private[this] val ascending: Ordering[(StopPrice, Number)] =
    (x: (StopPrice, Number), y: (StopPrice, Number)) => {
      import x.{_1 => xPrice, _2 => xNumber}
      import y.{_1 => yPrice, _2 => yNumber}
      val priceCompared = xPrice.compare(yPrice)
      if (priceCompared != 0) priceCompared
      else xNumber.compare(yNumber)
    }

  private[this] val descending: Ordering[(StopPrice, Number)] =
    (x: (StopPrice, Number), y: (StopPrice, Number)) => {
      import x.{_1 => xPrice, _2 => xNumber}
      import y.{_1 => yPrice, _2 => yNumber}
      val priceCompared = xPrice.compare(yPrice)
      if (priceCompared != 0) -priceCompared
      else xNumber.compare(yNumber)
    }

  val empty: OrdersRangeMap =
    new OrdersRangeMapImpl(
      orders = TreeMap.empty[(StopPrice, Number), Order](ascending),
      index = HashMap.empty[OrderId, (StopPrice, Number)],
      number = 0L
    )

  final private class OrdersRangeMapImpl(
    orders: TreeMap[(StopPrice, Number), Order],
    index: HashMap[OrderId, (StopPrice, Number)],
    number: Number
  ) extends OrdersRangeMap {

    override def contains(id: OrderId): Boolean =
      index.get(id).exists(orders.contains)

    override def get(id: OrderId): Option[Order] =
      index.get(id).flatMap(orders.get)

    override def range(price: StopPrice): Iterable[Order] =
      range(
        from = (price, 0L),
        until = (price, Long.MaxValue),
        ordering = ascending
      )

    override def range(begin: StopPrice, end: StopPrice): Iterable[Order] =
      if (begin <= end)
        range(
          from = (begin, Long.MaxValue),
          until = (end, Long.MaxValue),
          ordering = ascending
        )
      else
        range(
          from = (end, 0L),
          until = (begin, 0L),
          ordering = descending
        )

    private def range(
      from: (StopPrice, Number),
      until: (StopPrice, Number),
      ordering: Ordering[(StopPrice, Number)]
    ): Iterable[Order] =
      orders
        .range(from, until)
        .toVector
        .sortWith { case ((x, _), (y, _)) => ordering.compare(x, y) < 0 }
        .map { case (_, order) => order }

    override def updated(price: StopPrice, order: Order): OrdersRangeMap =
      new OrdersRangeMapImpl(
        orders = orders.updated((price, number), order),
        index = index.updated(order.id(), (price, number)),
        number = number + 1
      )

    override def removed(id: OrderId): OrdersRangeMap =
      index
        .get(id)
        .map { key =>
          new OrdersRangeMapImpl(
            orders = orders.removed(key),
            index = index.removed(id),
            number = number
          )
        }
        .getOrElse(this)

    override def removedAll(price: StopPrice): OrdersRangeMap =
      removedAll(
        from = (price, 0L),
        until = (price, Long.MaxValue)
      )

    override def removedAll(begin: StopPrice, end: StopPrice): OrdersRangeMap =
      if (begin <= end)
        removedAll(
          from = (begin, Long.MaxValue),
          until = (end, Long.MaxValue)
        )
      else
        removedAll(
          from = (end, 0L),
          until = (begin, 0L)
        )

    private def removedAll(
      from: (StopPrice, Number),
      until: (StopPrice, Number)
    ): OrdersRangeMap =
      orders.range(from, until) match {
        case range if range.isEmpty =>
          this
        case range =>
          new OrdersRangeMapImpl(
            orders = orders.removedAll(range.keys),
            index = index.removedAll(range.values.map(_.id())),
            number = number
          )
      }

  }

}
