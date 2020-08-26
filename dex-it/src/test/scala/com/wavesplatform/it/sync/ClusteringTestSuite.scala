package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it._
import com.wavesplatform.it.api.MatcherCommand
import com.wavesplatform.it.config.DexTestConfig.createAssetPair
import org.scalacheck.Gen

import scala.util.Random

class ClusteringTestSuite extends MatcherSuiteBase {
  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
        |  price-assets = ["WAVES"]
        |}
        |
        |akka {
        |  actor {
        |    provider = "cluster"
        |    allow-java-serialization = off
        |    serializers {
        |      myown = "com.wavesplatform.dex.api.http.routes.MySerialization"
        |    }
        |    serialization-bindings {
        |      "com.wavesplatform.dex.api.http.routes.WithMySerialization" = myown
        |    }
        |    serialization-identifiers {
        |      "com.wavesplatform.dex.api.http.routes.MySerialization" = 999
        |    }
        |  }
        |  remote.artery.canonical {
        |    port = 2551
        |  }
        |  cluster {
        |    downing-provider-class = "akka.cluster.sbr.SplitBrainResolverProvider"
        |    seed-nodes = [
        |      "akka://matcher@${getIp("dex-1")}:2551",
        |      "akka://matcher@${getIp("dex-2")}:2551"
        |    ]
        |  }
        |}""".stripMargin
    )

  private def arteryHostname(x: String): Config = ConfigFactory.parseString(s"""akka.remote.artery.canonical.hostname = "$x" """)

  private def dex1SuiteConfig = arteryHostname(getIp("dex-1")).withFallback(dexInitialSuiteConfig)
  private def dex2SuiteConfig = arteryHostname(getIp("dex-2")).withFallback(dexInitialSuiteConfig)

  override lazy val dex1: DexContainer = createDex(
    "dex-1",
    suiteInitialConfig = dex1SuiteConfig
  )

  protected lazy val dex2: DexContainer = createDex(
    "dex-2",
    suiteInitialConfig = dex2SuiteConfig
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    broadcastAndAwait(IssueEthTx, IssueWctTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, IssueEthTx.getQuantity / 2, eth),
      mkTransfer(bob, alice, IssueWctTx.getQuantity / 2, wct)
    )

    dex1.start()
    dex2.start()
  }

  "Place, fill and cancel a lot of orders" in {
    val placesNumber  = 30
    val cancelsNumber = placesNumber / 10

    val assetPairs = List(createAssetPair(eth, wct), ethWavesPair, wctWavesPair)

    val aliceOrders = mkOrders(alice, placesNumber, List(ethWavesPair))
    val bobOrders   = mkOrders(bob, placesNumber, List(wctWavesPair))

    val api = dex1.asyncApi

    val alicePlaces = aliceOrders.map(MatcherCommand.Place(api, _))
    val bobPlaces   = bobOrders.map(MatcherCommand.Place(api, _))
    val places      = Random.shuffle(alicePlaces ++ bobPlaces)

    // .toSet to remove duplications
    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).toSet.map(MatcherCommand.Cancel(api, alice, _))
    val bobCancels   = (1 to cancelsNumber).map(_ => choose(bobOrders)).toSet.map(MatcherCommand.Cancel(api, bob, _))
//    val cancels      = Random.shuffle(aliceCancels ++ bobCancels)

    executeCommands(places /*++ cancels*/)
  }

  private def mkOrders(account: KeyPair, number: Int, assetPairs: List[AssetPair]) = {
    Gen.containerOfN[Vector, Order](number, orderGen(matcher, account, assetPairs)).sample.get
  }
}
