package com.wavesplatform.it.sync

import cats.Id
import cats.instances.future._
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it._
import com.wavesplatform.it.api.{DexApi, HasWaitReady, MatcherCommand, MatcherState, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig
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
class MultipleMatchersTestSuite extends NewMatcherSuiteBase {
  private def configOverrides = ConfigFactory.parseString("""waves.dex {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  override protected def dex1Config: Config = configOverrides.withFallback(dexBaseConfig).withFallback(super.dex1Config)

  protected def dex2Config: Config                 = configOverrides.withFallback(dexBaseConfig).withFallback(DexTestConfig.containerConfig("dex-2"))
  protected def dex2NodeContainer: DockerContainer = wavesNode1Container()
  protected val dex2Container: Coeval[DexContainer] = Coeval.evalOnce {
    val grpcAddr = dockerClient().getInternalSocketAddress(dex2NodeContainer, dex2NodeContainer.config.getInt("waves.dex.grpc.integration.port"))
    val wavesNodeGrpcConfig = ConfigFactory
      .parseString(s"""waves.dex.waves-node-grpc {
                      |  host = ${grpcAddr.getAddress.getHostAddress}
                      |  port = ${grpcAddr.getPort}
                      |}""".stripMargin)
    val config = wavesNodeGrpcConfig.withFallback(dex2Config).withFallback(DexTestConfig.updatedMatcherConfig).resolve()
    dockerClient().createDex("dex-2", config)
  }

  protected def dex2AsyncApi: DexApi[Future] = {
    def apiAddress = dockerClient().getExternalSocketAddress(dex2Container(), dex2Config.getInt("waves.dex.rest-api.port"))
    DexApi[Future]("integration-test-rest-api", apiAddress)
  }

  protected def dex2Api: DexApi[Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(dex2Container(), dex2Config.getInt("waves.dex.rest-api.port"))
    fp.sync(DexApi[Try]("integration-test-rest-api", apiAddress))
  }

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
    broadcast(IssueEthTx, IssueWctTx)
    broadcast(
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
