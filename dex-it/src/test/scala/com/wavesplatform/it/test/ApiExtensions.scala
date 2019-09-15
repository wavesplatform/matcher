package com.wavesplatform.it.test

import cats.Id
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.api.NodeApi
import com.wavesplatform.dex.it.test.WavesNodeApiExtensions
import com.wavesplatform.it.api.dex._
import com.wavesplatform.it.api.{DexApi, MatcherState}
import com.wavesplatform.it.{MatcherSuiteBase, api}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}

import scala.collection.immutable.TreeMap

trait ApiExtensions extends WavesNodeApiExtensions {
  this: MatcherSuiteBase =>

  protected def placeAndAwait(order: Order, expectedStatus: OrderStatus = OrderStatus.Accepted): OrderStatusResponse = {
    dex1Api.place(order)
    dex1Api.waitForOrderStatus(order, expectedStatus)
  }

  protected def waitForOrderAtNode(order: Order, dexApi: DexApi[Id] = dex1Api, wavesNodeApi: NodeApi[Id] = wavesNode1Api): Id[ExchangeTransaction] =
    waitForOrderAtNode(order.id(), dexApi, wavesNodeApi)

  protected def waitForOrderAtNode(orderId: Order.Id, dexApi: DexApi[Id], wavesNodeApi: NodeApi[Id]): Id[ExchangeTransaction] = {
    val tx = dexApi.waitForTransactionsByOrder(orderId, 1).head
    wavesNodeApi.waitForTransaction(tx.id())
    tx
  }

  protected def matcherState(assetPairs: Seq[AssetPair],
                             orders: IndexedSeq[Order],
                             accounts: Seq[KeyPair],
                             dexApi: DexApi[Id] = dex1Api): MatcherState = {

    val offset               = dexApi.currentOffset
    val snapshots            = dexApi.allSnapshotOffsets
    val orderBooks           = assetPairs.map(x => (x, (dexApi.orderBook(x), dexApi.orderBookStatus(x))))
    val orderStatuses        = orders.map(x => x.idStr() -> dexApi.orderStatus(x))
    val reservedBalances     = accounts.map(x => x -> dexApi.reservedBalance(x))
    val accountsOrderHistory = accounts.flatMap(a => assetPairs.map(p => a -> p))
    val orderHistory = accountsOrderHistory.map {
      case (account, pair) => (account, pair, dexApi.orderHistoryByPair(account, pair))
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
