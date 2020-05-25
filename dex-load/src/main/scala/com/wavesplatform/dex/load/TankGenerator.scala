package com.wavesplatform.dex.load

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.AddressScheme
import com.wavesplatform.wavesj.{AssetPair, Node, PrivateKeyAccount, Transactions}

import scala.collection.mutable
import scala.util.Random

class Environment(conf: Config) {
  val node             = new Node(conf.getString("node"), AddressScheme.current.chainId)
  val matcherPublicKey = conf.getString("matcherPublicKey")
  val networkByte      = conf.getString("networkByte").charAt(0).toByte
  val issuer           = PrivateKeyAccount.fromSeed(conf.getString("bank"), 0, networkByte)

  val assetQuantity = conf.getLong("assets.quantity")
  val issueFee      = conf.getLong("assets.issueFee")
}

object TankGenerator {

  private def mkAccounts(seedPrefix: String, env: Environment, count: Int = 1000): List[PrivateKeyAccount] = {
    (for { i <- 1 to count } yield { PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, env.networkByte) }).toList
  }

  private def mkAssets(env: Environment, count: Int = 10): List[String] = {
    (for { _ <- 1 to count } yield {
      val tx =
        Transactions.makeIssueTx(
          env.issuer,
          env.networkByte,
          Random.nextInt(100000).toString,
          Random.nextInt(100000).toString,
          env.assetQuantity,
          8, //TODO: random from 2 to 16
          false,
          null,
          env.issueFee
        )
      println(tx.toString)
      env.node.send(tx)
      tx.getId.toString
    }).toList
  }

  private def mkAssetPairs(assets: List[String], count: Int = 10): mutable.HashSet[AssetPair] = {
    val pairs: mutable.HashSet[AssetPair] = mutable.HashSet()

    while (pairs.size <= count) pairs += new AssetPair(assets(Random.nextInt(assets.size - 1)), assets(assets.size - 1))
    pairs
  }

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String], env: Environment): Unit = {
    accounts
      .flatMap { acc =>
        Transactions.makeTransferTx(env.issuer, acc.getAddress, 10000000000L, "WAVES", 300000, "WAVES", "") :: assets.map { ass =>
          Transactions.makeTransferTx(env.issuer, acc.getAddress, 100000, ass, 300000, "WAVES", "")
        }
      }
      .flatMap(env.node.send(_))
  }

  private def mkPlaces(seedPrefix: String, env: Environment): Unit = {
    println("mkPlaces")
    val accounts = mkAccounts(seedPrefix, env, 100)
    val assets   = mkAssets(env)
    val pairs    = mkAssetPairs(assets)

    distributeAssets(accounts, assets, env)

//    val o = Transactions.makeOrder(PrivateKeyAccount.fromSeed("sds", 0, env.networkByte),
//                                   env.matcherPublicKey,
//                                   BUY,
//                                   pair,
//                                   1002332,
//                                   100500,
//                                   System.currentTimeMillis + 1005000,
//                                   300000)

  }

  private def mkCancels(seedPrefix: String, env: Environment): Unit = {
    println("mkCancels")
  }

  private def mkMatching(seedPrefix: String, env: Environment): Unit = {
    println("mkMatching")
  }

  private def mkOrderHistory(seedPrefix: String, env: Environment): Unit = {
    println("Wrong number")
  }

  private def mkAllTypes(): Unit = {
    println("mkAllTypes")
  }

  def mkRequests(seedPrefix: String, environment: String, task: Int = 1): Unit = {
    val env = new Environment(ConfigFactory.parseResources(environment))

    task match {
      case 1 => mkPlaces(seedPrefix, env)
      case 2 => mkCancels(seedPrefix, env)
      case 3 => mkMatching(seedPrefix, env)
      case 4 => mkOrderHistory(seedPrefix, env)
      case 5 => mkAllTypes
      case _ =>
        println("Wrong number")
    }

  }
}
