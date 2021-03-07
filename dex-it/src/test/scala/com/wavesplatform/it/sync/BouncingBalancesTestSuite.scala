package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsPingOrPong}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.LimitOrder
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.Assertion

class BouncingBalancesTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory
      .parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")
      .withFallback(jwtPublicKeyConfig)

  override def wavesNodeInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.miner.enable = false""".stripMargin)

  private val minerNodeSuiteConfig: Config = ConfigFactory.parseString("""waves.miner {
  enable = true
  micro-block-interval = 1s
  min-micro-block-age = 1s
}""")

  lazy val wavesMinerNode: WavesNodeContainer = createWavesNode("waves-2", suiteInitialConfig = minerNodeSuiteConfig, netAlias = None)

  private val IssueResults(issueDoggyCoinTx, _, doggyCoin) = mkIssueExtended(alice, "doggyCoin", 1000000.asset8)

  override protected def beforeAll(): Unit = {
    wavesMinerNode.start()
    broadcastAndAwait(wavesMinerNode.api, IssueUsdTx)

    wavesNode1.start()
    wavesNode1.api.connect(wavesMinerNode.networkAddress)
    wavesNode1.api.waitForConnectedPeer(wavesMinerNode.networkAddress)

    wavesNode1.api.waitForHeight(wavesMinerNode.api.currentHeight)
    wavesNode1.api.waitForTransaction(IssueUsdTx)

    dex1.start()
    log.info(s"\naddresses:\n alice=${alice.toAddress}\n bob=${bob.toAddress}\n\nassets:\n usd=$usd\n doggy=$doggyCoin")
  }

  "Balance should not bounce" - {
    "when a rollback is completed" in {
      val wsc = mkWsAddressConnection(bob, dex1)

      withClue("Bob has only Waves on balance\n") {
        eventually {
          val initialBobBalance = wsc.balanceChanges
          initialBobBalance should have size 1
          initialBobBalance.head should have size 1
          initialBobBalance.head.head._1 shouldBe Waves
          wsc.clearMessages()
        }
      }

      val heightInitial = wavesNode1.api.currentHeight
      val heightIssue = heightInitial + 1
      val heightFirstTransfer = heightIssue + 1
      val heightSecondTransfer = heightFirstTransfer + 1

      val doggyUsdPair = AssetPair(doggyCoin, usd)

      implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(assetDecimalsMap + (doggyCoin -> 8))

      // issue
      broadcastAndAwait(issueDoggyCoinTx)
      wavesNode1.api.waitForHeight(heightIssue)
      wavesNode1.api.currentHeight shouldBe heightIssue

      // first transfer, Bob's doggy balance = 500k
      broadcastAndAwait(mkTransfer(alice, bob, 500000.asset8, doggyCoin))
      wavesNode1.api.waitForHeight(heightFirstTransfer)
      wavesNode1.api.currentHeight shouldBe heightFirstTransfer

      assertChanges(wsc)(Map(doggyCoin -> WsBalances(0.0, 0.0)), Map(doggyCoin -> WsBalances(500000, 0.0)))()
      wsc.clearMessages()

      // second transfer, Bob's doggy balance = 1m
      broadcastAndAwait(mkTransfer(alice, bob, 500000.asset8, doggyCoin))
      wavesNode1.api.waitForHeight(heightSecondTransfer)
      wavesNode1.api.currentHeight shouldBe heightSecondTransfer

      assertChanges(wsc)(Map(doggyCoin -> WsBalances(1000000, 0.0)))()
      wsc.clearMessages()

      // Bob sells whole doggy balance = 1m
      val bobOrder = mkOrderDP(bob, doggyUsdPair, SELL, 1000000.asset8, 0.1)
      placeAndAwaitAtDex(bobOrder)

      assertChanges(wsc)(
        Map(Waves -> WsBalances(4949949.997, 0.003)), // Fee for order
        Map(doggyCoin -> WsBalances(0, 1000000))
      )(WsOrder.fromDomain(LimitOrder(bobOrder)))
      wsc.clearMessages()

      dex1.api.getOrderStatus(bobOrder).status shouldBe Status.Accepted

      withClue("After rollback order should not be cancelled and balances should not be decreased\n") {
        wavesNode1.asyncApi.rollback(heightInitial, returnTransactionsToUtx = true) // true as on Node
        wavesMinerNode.api.rollback(heightInitial, returnTransactionsToUtx = true) // true as on Node
        eventually {
          wavesNode1.api.currentHeight shouldBe >=(heightInitial)
        }

        wavesNode1.api.waitForHeight(heightSecondTransfer)
        wavesNode1.api.waitForHeightArise() // See WavesFork
        Thread.sleep(3000) // TODO We need an API to see the state on Matcher
        dex1.api.getOrderStatus(bobOrder).status shouldBe Status.Accepted

        //        wsc.messages.filter {
        //          case _: WsPingOrPong => false
        //          case _ => true
        //        } shouldBe empty

        // Relates DEX-1099
        dex1.api.getTradableBalance(bob, AssetPair(doggyCoin, Waves)) should matchTo(Map[Asset, Long](
          Waves -> 494994999700000L
        ))
      }

      wsc.close()
    }

    "multiple orders test" in {
      val aliceWsc = mkWsAddressConnection(alice, dex1)
      val bobWsc = mkWsAddressConnection(bob, dex1)

      val heightInitial = wavesNode1.api.currentHeight

      val now = System.currentTimeMillis()
      val counterOrders = (1 to 25).map(i => mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 1.waves, 10, ts = now + i))
      val submittedOrders = (1 to 50).map(i => mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 0.5.waves, 10, ts = now + i))

      counterOrders.foreach(dex1.api.place)
      submittedOrders.foreach(dex1.api.place)

      counterOrders.foreach(dex1.api.waitForOrderStatus(_, Status.Filled))
      submittedOrders.foreach(dex1.api.waitForOrderStatus(_, Status.Filled))

      def checkOrdering(label: String, xs: List[Double])(cmp: (Double, Double) => Assertion): Unit = {
        val elementsStr = xs.zipWithIndex.map { case (x, i) => s"$i: $x" }.mkString(", ")
        withClue(s"$label ($elementsStr):\n") {
          xs.zip(xs.tail).zipWithIndex.foreach {
            case ((b1, b2), i) => withClue(s"$i: ")(cmp(b1, b2))
          }
        }
      }

      val aliceUsdChanges = collectTradableBalanceChanges(aliceWsc, usd)
      checkOrdering("alice usd", aliceUsdChanges)(_ shouldBe >=(_))

      // probably we need to zip with aliceUsdChanges and check
      //  val aliceWavesChanges = collectTradableBalanceChanges(aliceWsc, Waves)
      //  checkOrdering("alice Waves", aliceWavesChanges)(_ shouldBe <=(_))

      //  val bobUsdChanges = collectTradableBalanceChanges(bobWsc, usd)
      //  checkOrdering("bob usd", bobUsdChanges)(_ shouldBe <=(_))

      val bobWavesChanges = collectTradableBalanceChanges(bobWsc, Waves)
      checkOrdering("bob Waves", bobWavesChanges)(_ shouldBe >=(_))

      submittedOrders.foreach(waitForOrderAtNode(_))
      val finalHeight = wavesNode1.api.waitForHeightArise()
      val bobBalanceBefore = dex1.api.getTradableBalance(bob, wavesUsdPair)

      step("Doing a rollback")
      wavesNode1.asyncApi.rollback(heightInitial, returnTransactionsToUtx = true) // true as on Node
      wavesMinerNode.api.rollback(heightInitial, returnTransactionsToUtx = true) // true as on Node
      eventually {
        wavesNode1.api.currentHeight shouldBe >=(heightInitial)
      }

      step("Wait for a height to be restored")
      wavesNode1.api.waitForHeight(finalHeight)
      wavesNode1.api.waitForHeightArise() // See WavesFork
      Thread.sleep(3000)

      // Relates DEX-1099
      eventually {
        dex1.api.getTradableBalance(bob, wavesUsdPair) should matchTo(bobBalanceBefore)
      }

      aliceWsc.close()
      bobWsc.close()
    }
  }

  private def collectTradableBalanceChanges(wsc: WsConnection, asset: Asset): List[Double] = wsc.messages.collect {
    case x: WsAddressChanges if x.balances.contains(asset) => x.balances(asset).tradable
  }

}
