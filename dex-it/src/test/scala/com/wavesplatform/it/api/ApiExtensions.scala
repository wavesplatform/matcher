package com.wavesplatform.it.api

import java.util.concurrent.ThreadLocalRandom

import cats.Id
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.{HttpOrderBookHistoryItem, HttpOrderStatus}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.api.node.{NodeApi, NodeApiExtensions}
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.{MatcherSuiteBase, api}
import im.mak.waves.transactions.ExchangeTransaction
import mouse.any._

import scala.collection.immutable.TreeMap
import scala.collection.parallel.CollectionConverters._

trait ApiExtensions extends NodeApiExtensions {
  this: MatcherSuiteBase =>

  protected def placeAndAwaitAtDex(
    order: Order,
    expectedStatus: HttpOrderStatus.Status = Status.Accepted,
    dex: DexContainer = dex1,
    isMarketOrder: Boolean = false
  ): HttpOrderStatus = {
    if (isMarketOrder) dex.api.placeMarket(order) else dex.api.place(order)
    dex.api.waitForOrderStatus(order, expectedStatus)
  }

  protected def placeAndAwaitAtNode(
    order: Order,
    dexApi: DexApi[Id] = dex1.api,
    wavesNodeApi: NodeApi[Id] = wavesNode1.api,
    isMarketOrder: Boolean = false
  ): Seq[ExchangeTransaction] = {
    if (isMarketOrder) dexApi.placeMarket(order) else dexApi.place(order)
    waitForOrderAtNode(order.id(), dexApi, wavesNodeApi)
  }

  protected def cancelAndAwait(owner: KeyPair, order: Order, expectedStatus: HttpOrderStatus.Status = Status.Cancelled): HttpOrderStatus = {
    dex1.api.cancel(owner, order)
    dex1.api.waitForOrderStatus(order, expectedStatus)
  }

  protected def waitForOrderAtNode(
    order: Order,
    dexApi: DexApi[Id] = dex1.api,
    wavesNodeApi: NodeApi[Id] = wavesNode1.api
  ): Seq[ExchangeTransaction] =
    waitForOrderAtNode(order.id(), dexApi, wavesNodeApi)

  protected def waitForOrderAtNode(orderId: Order.Id, dexApi: DexApi[Id], wavesNodeApi: NodeApi[Id]): Seq[ExchangeTransaction] =
    dexApi.waitForTransactionsByOrder(orderId, 1).unsafeTap {
      _.foreach(tx => wavesNodeApi.waitForTransaction(tx.id()))
    }

  protected def matcherState(
    assetPairs: Seq[AssetPair],
    orders: IndexedSeq[Order],
    accounts: Seq[KeyPair],
    dexApi: DexApi[Id] = dex1.api
  ): MatcherState = {

    val offset = dexApi.getCurrentOffset
    val snapshots = dexApi.getAllSnapshotOffsets
    val orderBooks = assetPairs.map(x => (x, (dexApi.getOrderBook(x), dexApi.getOrderBookStatus(x))))
    val orderStatuses = orders.map(x => x.idStr() -> dexApi.getOrderStatus(x))
    val orderTransactionIds = orders.map(x => x.idStr() -> dexApi.getTransactionsByOrder(x).map(_.id().toString).toSet)
    val reservedBalances = accounts.map(x => x -> dexApi.getReservedBalance(x))
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

          account -> (TreeMap.empty[AssetPair, Seq[HttpOrderBookHistoryItem]] ++ assetPairHistory)
      }

    clean {
      api.MatcherState(
        offset,
        TreeMap(snapshots.toSeq: _*),
        TreeMap(orderBooks: _*),
        TreeMap(orderStatuses: _*),
        TreeMap(orderTransactionIds: _*),
        TreeMap(reservedBalances: _*),
        TreeMap(orderHistoryMap.toSeq: _*)
      )
    }
  }

  private def clean(x: MatcherState): MatcherState = x.copy(
    orderBooks = x.orderBooks.map { case (k, v) => k -> v.copy(_1 = v._1.copy(timestamp = 0L)) }
  )

  def mkAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
    val account = mkKeyPair(s"account-test-${ThreadLocalRandom.current().nextInt}")
    val transfers = balances.map {
      case (balance, asset) =>
        val sender = if (wavesNode1.api.balance(alice, asset) >= balance) alice else bob
        mkTransfer(sender, account, balance, asset, 0.003.waves)
    }
    transfers.par.foreach(broadcastAndAwait(_))
    eventually {
      balances.foreach {
        case (expectedBalance, asset) => wavesNode1.api.balance(account, asset) shouldBe expectedBalance
      }
    }
    account
  }

  implicit private val assetPairOrd: Ordering[AssetPair] = Ordering.by[AssetPair, String](_.key)
  implicit private val keyPairOrd: Ordering[KeyPair] = Ordering.by[KeyPair, String](_.stringRepr)
}
