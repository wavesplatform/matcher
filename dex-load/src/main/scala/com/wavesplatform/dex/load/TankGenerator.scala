package com.wavesplatform.dex.load

import akka.util.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.AddressScheme
import com.wavesplatform.wavesj.{Node, PrivateKeyAccount, Transactions}

import scala.util.Random

class Environment(conf: Config) {
  val node        = new Node(conf.getString("node"), AddressScheme.current.chainId)
  val networkByte = conf.getString("schema").toByte
  val issuer      = PrivateKeyAccount.fromSeed(conf.getString("bank"), 0, networkByte)
}

object TankGenerator {

  private def selectTask(): Int = {
    println("Print number of the task: \n\t1. Places \n\t2. Places & Cancels \n\t3. Matching \n\t4. Order History \n\t5. All of types")
    scala.io.StdIn.readInt()
  }

  private def mkAssets(env: Environment, count: Int = 10): List[ByteString] = {
    (for { _ <- 1 to count } yield {
      val tx =
        Transactions.makeIssueTx(env.issuer, env.networkByte, Random.nextString(10), Random.nextString(10), 100000000, 8, false, "", 300000)
      env.node.send(tx)
      tx.getId
    }).toList
  }

  private def mkAssetPairs() = {}

  private def mkPlaces(seedPrefix: String, env: Environment): Unit = {
    println("mkPlaces")
    val assets = mkAssets(env)
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

  def mkRequests(seedPrefix: String, environment: String): Unit = {
    val env = new Environment(ConfigFactory.parseResources(environment))

    selectTask match {
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
