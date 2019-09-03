package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.{MatcherCommand, MatcherState}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.assets.exchange.Order
import org.scalacheck.Gen

import scala.util.Random

class MatcherRecoveryTestSuite extends NewMatcherSuiteBase {
  override protected def dex1Config: Config = ConfigFactory.parseString("waves.dex.snapshots-interval = 51").withFallback(super.dex1Config)

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val assetPairs = List(ethUsdPair, wavesUsdPair, ethWavesPair)
  private val orders     = Gen.containerOfN[Vector, Order](placesNumber, orderGen(matcher, alice, assetPairs)).sample.get
  private val lastOrder  = orderGen(matcher, alice, assetPairs).sample.get

  private var successfulCommandsNumber = 0

  "Place, fill and cancel a lot of orders" in {
    val cancels  = (1 to cancelsNumber).map(_ => choose(orders))
    val commands = Random.shuffle(orders.map(MatcherCommand.Place(dex1AsyncApi, _))) ++ cancels.map(MatcherCommand.Cancel(dex1AsyncApi, alice, _))
    successfulCommandsNumber += executeCommands(commands)
    successfulCommandsNumber += executeCommands(List(MatcherCommand.Place(dex1AsyncApi, lastOrder)))
  }

  "Wait until all requests are processed - 1" in {
    dex1Api.waitForCurrentOffset(_ == successfulCommandsNumber - 1) // Index starts from 0
  }

  private var stateBefore: MatcherState = _

  "Store the current state" in {
    stateBefore = state
    withClue("common offset") {
      stateBefore.offset should be > 0L
    }
    stateBefore.snapshots.foreach {
      case (assetPair, snapshotOffset) =>
        withClue(assetPair) {
          snapshotOffset should be > 0L
        }
    }
  }

  "Restart the matcher" in restartContainer(dex1Container(), dex1Api)

  "Wait until all requests are processed - 2" in dex1Api.waitForCurrentOffset(_ == successfulCommandsNumber - 1)

  "Verify the state" in {
    val stateAfter = state
    stateBefore shouldBe stateAfter
  }

  private def state = cleanState(matcherState(assetPairs, orders, Seq(alice)))

  protected def cleanState(state: MatcherState): MatcherState = state

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueEthTx, IssueUsdTx)
  }
}
