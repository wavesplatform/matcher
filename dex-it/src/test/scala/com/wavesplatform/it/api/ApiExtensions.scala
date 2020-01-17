package com.wavesplatform.it.api

import cats.Id
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.api.node.{NodeApi, NodeApiExtensions}
import com.wavesplatform.dex.it.api.responses.dex.{OrderBookHistoryItem, OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.{MatcherSuiteBase, api}
import com.wavesplatform.wavesj.transactions.ExchangeTransaction
import mouse.any._

import scala.collection.immutable.TreeMap

trait ApiExtensions extends NodeApiExtensions { this: MatcherSuiteBase =>

  protected def placeAndAwaitAtDex(order: Order, expectedStatus: OrderStatus = OrderStatus.Accepted): OrderStatusResponse = {
    dex1.api.place(order)
    dex1.api.waitForOrderStatus(order, expectedStatus)
  }

  protected def placeAndAwaitAtNode(order: Order,
                                    dexApi: DexApi[Id] = dex1.api,
                                    wavesNodeApi: NodeApi[Id] = wavesNode1.api,
                                    isMarketOrder: Boolean = false): Seq[ExchangeTransaction] = {
    if (isMarketOrder) dex1.api.placeMarket(order) else dex1.api.place(order)
    waitForOrderAtNode(order.id(), dexApi, wavesNodeApi)
  }

  protected def cancelAndAwait(owner: KeyPair, order: Order, expectedStatus: OrderStatus = OrderStatus.Cancelled): OrderStatusResponse = {
    dex1.api.cancel(owner, order)
    dex1.api.waitForOrderStatus(order, expectedStatus)
  }

  protected def waitForOrderAtNode(order: Order,
                                   dexApi: DexApi[Id] = dex1.api,
                                   wavesNodeApi: NodeApi[Id] = wavesNode1.api): Seq[ExchangeTransaction] =
    waitForOrderAtNode(order.id(), dexApi, wavesNodeApi)

  protected def waitForOrderAtNode(orderId: Order.Id, dexApi: DexApi[Id], wavesNodeApi: NodeApi[Id]): Seq[ExchangeTransaction] =
    dex1.api.waitForTransactionsByOrder(orderId, 1).unsafeTap {
      _.foreach(tx => wavesNodeApi.waitForTransaction(tx.getId))
    }

  protected def matcherState(assetPairs: Seq[AssetPair],
                             orders: IndexedSeq[Order],
                             accounts: Seq[KeyPair],
                             dexApi: DexApi[Id] = dex1.api): MatcherState = {

    val offset               = dex1.api.currentOffset
    val snapshots            = dex1.api.allSnapshotOffsets
    val orderBooks           = assetPairs.map(x => (x, (dex1.api.orderBook(x), dex1.api.orderBookStatus(x))))
    val orderStatuses        = orders.map(x => x.idStr() -> dex1.api.orderStatus(x))
    val reservedBalances     = accounts.map(x => x -> dex1.api.reservedBalance(x))
    val accountsOrderHistory = accounts.flatMap(a => assetPairs.map(p => a -> p))

    val orderHistory = accountsOrderHistory.map {
      case (account, pair) => (account, pair, dex1.api.orderHistoryByPair(account, pair))
    }

    val orderHistoryMap = orderHistory
      .groupBy(_._1) // group by accounts
      .map {
        case (account, xs) =>
          val assetPairHistory = xs.groupBy(_._2).map { // group by asset pair
            case (assetPair, historyRecords) => assetPair -> historyRecords.flatMap(_._3) // same as historyRecords.head._3
          }

          account -> (TreeMap.empty[AssetPair, Seq[OrderBookHistoryItem]] ++ assetPairHistory)
      }

    clean {
      api.MatcherState(offset,
                       TreeMap(snapshots.toSeq: _*),
                       TreeMap(orderBooks: _*),
                       TreeMap(orderStatuses: _*),
                       TreeMap(reservedBalances: _*),
                       TreeMap(orderHistoryMap.toSeq: _*))
    }
  }

  private def clean(x: MatcherState): MatcherState = x.copy(
    orderBooks = x.orderBooks.map { case (k, v) => k -> v.copy(_1 = v._1.copy(timestamp = 0L)) }
  )

  private implicit val assetPairOrd: Ordering[AssetPair] = Ordering.by[AssetPair, String](_.key)
  private implicit val keyPairOrd: Ordering[KeyPair]     = Ordering.by[KeyPair, String](_.stringRepr)
}
