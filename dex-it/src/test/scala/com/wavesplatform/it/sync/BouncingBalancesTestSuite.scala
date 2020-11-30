package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.WsPingOrPong
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.LimitOrder
import com.wavesplatform.it.WsSuiteBase

class BouncingBalancesTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory
      .parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")
      .withFallback(jwtPublicKeyConfig)

  override def wavesNodeInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.miner.enable = false""".stripMargin)

  private val minerNodeSuiteConfig: Config = ConfigFactory.parseString("waves.miner.enable = true")

  lazy val wavesMinerNode: WavesNodeContainer = createWavesNode("waves-2", suiteInitialConfig = minerNodeSuiteConfig, netAlias = None)

  override protected def beforeAll(): Unit = {
    wavesMinerNode.start()
    broadcastAndAwait(wavesMinerNode.api, IssueUsdTx)

    wavesNode1.start()
    wavesNode1.api.connect(wavesMinerNode.networkAddress)
    wavesNode1.api.waitForConnectedPeer(wavesMinerNode.networkAddress)

    wavesNode1.api.waitForHeight(wavesMinerNode.api.currentHeight)
    wavesNode1.api.waitForTransaction(IssueUsdTx)

    dex1.start()
  }

  "Balance should not bounce when" - {
    "rollback is completed" in {

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

      val IssueResults(issueDoggyCoinTx, _, doggyCoin) = mkIssueExtended(alice, "doggyCoin", 1000000.asset8)
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
        Map(Waves -> WsBalances(4949949.997, 0.003)),
        Map(doggyCoin -> WsBalances(0, 1000000))
      )(WsOrder.fromDomain(LimitOrder(bobOrder)))
      wsc.clearMessages()

      dex1.api.getOrderStatus(bobOrder).status shouldBe Status.Accepted

      withClue(s"After rollback order should not be cancelled and balances should not be decreased\n") {
        wavesNode1.api.rollback(heightInitial, returnTransactionsToUtx = false)
        wavesNode1.api.currentHeight shouldBe heightInitial

        wavesNode1.api.waitForHeight(heightSecondTransfer)
        dex1.api.getOrderStatus(bobOrder).status shouldBe Status.Accepted

        wsc.messages.filter {
          case _: WsPingOrPong => false
          case _ => true
        } shouldBe empty
      }
    }
  }
}
