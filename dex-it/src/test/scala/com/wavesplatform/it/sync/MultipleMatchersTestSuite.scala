package com.wavesplatform.it.sync

import cats.Id
import cats.instances.future._
import cats.instances.try_._
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.fp
import com.wavesplatform.it._
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.it.api.{DexApi, MatcherCommand, MatcherState}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.docker.{DexContainer, DockerContainer}
import com.wavesplatform.it.tags.DexItKafkaRequired
import com.wavesplatform.transaction.assets.exchange.Order
import monix.eval.Coeval
import org.scalacheck.Gen

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Random, Try}

@DexItKafkaRequired
class MultipleMatchersTestSuite extends MatcherSuiteBase {
  override protected val suiteInitialDexConfig = ConfigFactory.parseString("""waves.dex {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  protected val dex2Container: Coeval[DexContainer] = Coeval.evalOnce {
    dockerClient.createDex("dex-2", dexRunConfig(), suiteInitialDexConfig)
  }

  private def dex2ApiAddress                 = dockerClient.getExternalSocketAddress(dex2Container(), dex2Container().restApiPort)
  protected def dex2AsyncApi: DexApi[Future] = DexApi[Future]("integration-test-rest-api", dex2ApiAddress)
  protected def dex2Api: DexApi[Id]          = fp.sync(DexApi[Try]("integration-test-rest-api", dex2ApiAddress))

  override protected def allContainers: List[DockerContainer] = dex2Container() :: super.allContainers
  override protected def allApis: List[HasWaitReady[Id]]      = dex2Api :: super.allApis

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val assetPairs = Seq(createAssetPair(eth, wct), ethWavesPair, wctWavesPair)

  private val aliceOrders = mkOrders(alice)
  private val bobOrders   = mkOrders(bob)
  private val orders      = aliceOrders ++ bobOrders
  private val lastOrder   = orderGen(matcher, alice, assetPairs).sample.get

  private var successfulCommandsNumber = 0

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx, IssueWctTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, IssueEthTx.quantity / 2, eth),
      mkTransfer(bob, alice, IssueWctTx.quantity / 2, wct)
    )
  }

  "Place, fill and cancel a lot of orders" in {
    val alicePlaces = aliceOrders.map(MatcherCommand.Place(dex1AsyncApi, _))
    val bobPlaces   = bobOrders.map(MatcherCommand.Place(dex2AsyncApi, _))
    val places      = Random.shuffle(alicePlaces ++ bobPlaces)

    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).map(MatcherCommand.Cancel(dex1AsyncApi, alice, _))
    val bobCancels   = (1 to cancelsNumber).map(_ => choose(bobOrders)).map(MatcherCommand.Cancel(dex2AsyncApi, bob, _))
    val cancels      = Random.shuffle(aliceCancels ++ bobCancels)

    successfulCommandsNumber = executeCommands(places ++ cancels)
    successfulCommandsNumber += executeCommands(List(MatcherCommand.Place(dex1AsyncApi, lastOrder)))
  }

  "Wait until all requests are processed" in {
    try {
      val offset1 = dex1Api.waitForCurrentOffset(_ == successfulCommandsNumber - 1) // Index starts from 0
      dex2Api.waitForCurrentOffset(_ == offset1)

      withClue("Last command processed") {
        List(dex1AsyncApi, dex2AsyncApi).foreach(_.waitForOrder(lastOrder)(_.status != OrderStatus.NotFound))
      }
    } catch {
      case NonFatal(e) =>
        log.info(s"Last offsets: node1=${dex1Api.lastOffset}, node2=${dex2Api.lastOffset}")
        throw e
    }
  }

  "States on both matcher should be equal" in {
    val state1 = state(dex1Api)
    val state2 = state(dex2Api)
    state1 shouldBe state2
  }

  private def mkOrders(account: KeyPair) = Gen.containerOfN[Vector, Order](placesNumber, orderGen(matcher, account, assetPairs)).sample.get
  private def state(dexApi: DexApi[Id])  = clean(matcherState(assetPairs, orders, Seq(alice), dexApi))

  // Because we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
  private def clean(state: MatcherState): MatcherState = state.copy(
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )
}
