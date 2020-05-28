package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.matcher.{CancelOrder, Order}
import com.wavesplatform.wavesj.{AssetPair, PrivateKeyAccount, Transactions}

import scala.collection.mutable
import scala.util.Random

object TankGenerator extends ScorexLogging {
  private def mkAccounts(seedPrefix: String, env: Environment, count: Int): List[PrivateKeyAccount] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    val accounts = (1 to count).map(i => PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, env.networkByte)).toList
    println("Done")
    accounts
  }

  private def mkAssets(env: Environment, count: Int = 9): List[String] = {
    println(s"Generating $count assets... ")
    val assets = (1 to count).map(_ => mkAsset(env)).toList
    waitForHeightArise(env)
    println("Assets have been successfully issued")
    assets
  }

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String], env: Environment): Unit = {
    println(s"Distributing assets to accounts... ")
    val amountPerUser = env.assetQuantity / 2 / accounts.length
    accounts
      .flatMap { acc =>
        Transactions.makeTransferTx(env.issuer, acc.getAddress, 10000000000L, "WAVES", 300000, "WAVES", "") :: assets.map { ass =>
          Transactions.makeTransferTx(env.issuer, acc.getAddress, amountPerUser, ass, 300000, "WAVES", "")
        }
      }
      .flatMap(env.node.send(_))

    waitForHeightArise(env)
    println("Done")
  }

  private def mkAssetPairs(assets: List[String], count: Int = 10): List[AssetPair] = {
    print(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle(assets)
      .combinations(2)
      .map { case List(aa, pa) => if (aa > pa) (aa, pa) else (pa, aa) }
      .map(Function.tupled(new AssetPair(_, _)))

    println("Done")
    randomAssetPairs.take(count).toList
  }

  private def mkOrders(env: Environment, accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean = true): List[Order] = {
    print(s"Creating orders... ")
    val safeAmount = env.assetQuantity / 2 / accounts.length / 400

    val orders = accounts.map(
      mkOrder(env,
              _,
              if (math.random < 0.5) Type.BUY else Type.SELL,
              Random.nextInt(safeAmount.toInt),
              Random.nextInt(safeAmount.toInt),
              pairs(Random.nextInt(pairs.length))))

    println("Done")
    Random.shuffle(orders)
  }

  private def mkPlaces(seedPrefix: String, env: Environment, requestsCount: Int): Unit = {
    print("Making requests for placing...")
    val accounts = mkAccounts(seedPrefix, env, requestsCount / 400)
    val assets   = mkAssets(env)
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(env, accounts, pairs, false)

    distributeAssets(accounts, assets, env)
    svRequests(env, orders)
  }

  private def mkCancels(seedPrefix: String, env: Environment, requestsCount: Int): Unit = {
    println("Making requests for cancelling...")
    val accounts                              = mkAccounts(seedPrefix, env, requestsCount / 400)
    val cancels: mutable.HashSet[CancelOrder] = mutable.HashSet()

    accounts.foreach(a => {
      env.matcher
        .getOrders(a)
        .forEach(o => {
          cancels += Transactions.makeOrderCancel(a, o.getAssetPair, o.getId.getBase58String)
        })
    })

    svRequests(env, cancels = cancels.toList)
  }

  private def mkMatching(seedPrefix: String, env: Environment, requestsCount: Int): Unit = {
    println("Making requests for matching...")
    val accounts = mkAccounts(seedPrefix, env, requestsCount / 400)
    val assets   = mkAssets(env)
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(env, accounts, pairs)

    distributeAssets(accounts, assets, env)
    svRequests(env, orders)
  }

  private def mkOrderHistory(seedPrefix: String, env: Environment, requestsCount: Int): Unit = {
    println("Making requests for getting order history...")
  }

  private def mkAllTypes(): Unit = {
    println("mkAllTypes")
  }

  private def svRequests(env: Environment, orders: List[Order] = List.empty, cancels: List[CancelOrder] = List.empty): Unit = {
    println("All data has been generated. Now it will be saved...")
    val requestsFile = new File(s"requests-${System.currentTimeMillis}.txt")
    val output       = new PrintWriter(requestsFile, "utf-8")
    try {
      orders.foreach(o => {
        output.println(mkPost(env, o, "/matcher/orderbook", "PLACE"))
      })
    } finally output.close()
    println(s"Generated orders count: ${orders.length}")
    println(s"Generated cancels count: ${cancels.length}")
    println(s"Results have been saved to $requestsFile")
  }

  def mkRequests(seedPrefix: String, environment: String, requestsType: Int = 2, requestsCount: Int = 20000): Unit = {
    val env = new Environment(ConfigFactory.parseResources(environment))

    requestsType match {
      case 1 => mkPlaces(seedPrefix, env, requestsCount)
      case 2 => mkCancels(seedPrefix, env, requestsCount)
      case 3 => mkMatching(seedPrefix, env, requestsCount)
      case 4 => mkOrderHistory(seedPrefix, env, requestsCount)
      case 5 => mkAllTypes()
      case _ =>
        println("Wrong number of task ")
    }
  }
}
