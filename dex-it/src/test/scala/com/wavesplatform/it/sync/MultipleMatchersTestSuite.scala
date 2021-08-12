package com.wavesplatform.it.sync

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.Implicits.durationToScalatestTimeout
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulBatchCancel
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsOrderBookChanges}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.OrderCanceled
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it._
import com.wavesplatform.it.api.{MatcherCommand, MatcherState}
import com.wavesplatform.it.config.DexTestConfig.createAssetPair
import com.wavesplatform.it.tags.DexItExternalKafkaRequired
import org.scalacheck.Gen

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Random, Using}
import scala.util.control.NonFatal

@DexItExternalKafkaRequired
class MultipleMatchersTestSuite extends MatcherSuiteBase with HasWebSockets with WsSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory
      .parseString(
        """waves.dex {
          |  price-assets = ["WAVES"]
          |  snapshots-interval = 51
          |}""".stripMargin
      )
      .withFallback(jwtPublicKeyConfig)

  protected lazy val dex2: DexContainer = createDex("dex-2")

  private val placesNumber = 200
  private val cancelsNumber = placesNumber / 10

  private val assetPairs = Seq(createAssetPair(eth, wct), ethWavesPair, wctWavesPair)

  private val aliceOrders = mkOrders(alice)
  private val bobOrders = mkOrders(bob)
  private val orders = aliceOrders ++ bobOrders
  private val lastOrder = orderGen(matcher, alice, assetPairs).sample.get

  private var successfulCommandsNumber = 0

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    broadcastAndAwait(IssueEthTx, IssueWctTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, IssueEthTx.quantity() / 2, eth),
      mkTransfer(bob, alice, IssueWctTx.quantity() / 2, wct)
    )

    dex1.start()
    dex2.start()
  }

  "Dex 2 should continue getting the balance changes if Dex 1 has been disconnected from network" in {
    val acc = mkAccountWithBalance(10.waves -> Waves)
    dex1.disconnectFromNetwork()
    broadcastAndAwait(mkTransfer(acc, alice.toAddress, 4.waves, Waves, 0.05.waves))
    dex2.api.getTradableBalanceByAssetPairAndAddress(acc, ethWavesPair)(Waves) shouldBe 5.95.waves
    dex1.connectToNetwork()
  }

  "Place, fill and cancel a lot of orders" in {

    val alicePlaces = aliceOrders.map(MatcherCommand.Place(dex1, _))
    val bobPlaces = bobOrders.map(MatcherCommand.Place(dex2, _))
    val places = Random.shuffle(alicePlaces ++ bobPlaces)

    // .toSet to remove duplications
    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).toSet.map(MatcherCommand.Cancel(dex1, alice, _))
    val bobCancels = (1 to cancelsNumber).map(_ => choose(bobOrders)).toSet.map(MatcherCommand.Cancel(dex2, bob, _))
    val cancels = Random.shuffle(aliceCancels ++ bobCancels)

    successfulCommandsNumber = executeCommands(places)
    // We have to do this separately because an order could be in progress (during placement).
    //  So we could cancel it, but later.
    //  But instead we receive OrderNotFound.
    //    And this is the different case than OrderNotFound in an order book (executed),
    //    because we didn't saved it to the queue.
    successfulCommandsNumber += executeCommands(cancels.toSeq)

    successfulCommandsNumber += executeCommands(List(MatcherCommand.Place(dex1, lastOrder)))
    log.info(s"Successful commands: $successfulCommandsNumber")
  }

  "Wait until all requests are processed" in {
    val expectedOffset = successfulCommandsNumber - 1
    try {
      val offset1 = dex1.api.waitForCurrentOffset(_ == expectedOffset) // Index starts from 0
      dex2.api.waitForCurrentOffset(_ == offset1)

      withClue("Last command processed") {
        List(dex1.asyncApi, dex2.asyncApi).foreach(_.waitForOrder(lastOrder)(_.status != Status.NotFound))
      }
    } catch {
      case NonFatal(e) =>
        log.info(s"Last offsets: node1=${dex1.api.getLastOffset}, node2=${dex2.api.getLastOffset}, expected=$expectedOffset")
        throw e
    }
  }

  "States on both matcher should be equal" in {
    val state1 = state(dex1.api)
    val state2 = state(dex2.api)

    state1 should matchTo(state2)
  }

  "WS Order book state should be the same on two matchers" in {
    val acc = mkAccountWithBalance(100.eth -> eth, 100.waves -> Waves)

    Using.Manager.unsafe { use =>
      val wsob1 = use(mkWsOrderBookConnection(ethWavesPair, dex1))
      val wsob2 = use(mkWsOrderBookConnection(ethWavesPair, dex2))

      val sell = mkOrder(acc, ethWavesPair, SELL, 10.eth, 1.waves, 0.003.waves)
      dex1.api.place(sell)

      List(
        mkOrder(alice, ethWavesPair, BUY, 5.eth, 1.waves, 0.003.waves),
        mkOrder(alice, ethWavesPair, BUY, 3.eth, 1.waves, 0.003.waves),
        mkOrder(alice, ethWavesPair, BUY, 2.eth, 1.waves, 0.003.waves)
      ).foreach(dex1.api.place)

      dex1.api.waitForOrderStatus(sell, Status.Filled)

      eventually {
        val obs1 = wsob1.receiveAtLeastN[WsOrderBookChanges](1).reduce(mergeOrderBookChanges)
        val obs2 = wsob2.receiveAtLeastN[WsOrderBookChanges](1).reduce(mergeOrderBookChanges)

        obs1 should matchTo(obs2)
      }
    }
  }

  "WS Address state should be the same on two matchers" in {
    val acc = mkAccountWithBalance(100.eth -> eth, 100.waves -> Waves)

    Using.Manager.unsafe { use =>
      val wsau1 = use(mkWsAddressConnection(acc, dex1))
      val wsau2 = use(mkWsAddressConnection(acc, dex2))
      val sell = mkOrder(acc, ethWavesPair, SELL, 10.eth, 1.waves, 0.003.waves)
      dex1.api.place(sell)

      List(
        mkOrder(alice, ethWavesPair, BUY, 5.eth, 1.waves, 0.003.waves),
        mkOrder(alice, ethWavesPair, BUY, 3.eth, 1.waves, 0.003.waves),
        mkOrder(alice, ethWavesPair, BUY, 2.eth, 1.waves, 0.003.waves)
      ).foreach(dex1.api.place)

      dex1.api.waitForOrderStatus(sell, Status.Filled)

      eventually {
        val aus1 = wsau1.receiveAtLeastN[WsAddressChanges](1).reduce(mergeAddressChanges)
        val aus2 = wsau2.receiveAtLeastN[WsAddressChanges](1).reduce(mergeAddressChanges)

        aus1 should matchTo(aus2)
      }
    }
  }

  "Batch cancel and single cancels simultaneously" in {

    dex1.api.cancelAllOrdersWithSig(alice)
    dex1.api.cancelAllOrdersWithSig(bob)

    val allOrders =
      (Gen.containerOfN[Vector, Order](150, orderGen(matcher, bob, assetPairs, Seq(OrderType.BUY))).sample.get ++
        Gen.containerOfN[Vector, Order](150, orderGen(matcher, alice, assetPairs, Seq(OrderType.BUY))).sample.get).toSet

    log.info(s"Total orders: ${allOrders.size}")

    allOrders.foreach(dex1.api.place)
    allOrders.foreach(order => dex1.api.waitForOrder(order)(_.status != Status.NotFound))

    def singleCancels(owner: KeyPair, orders: Iterable[Order]): Future[Unit] =
      Future
        .sequence {
          orders.map { order =>
            dex1.asyncTryApi.cancelOneOrAllInPairOrdersWithSig(owner, order).map {
              case Left(x) if x.error != OrderCanceled.code => throw new RuntimeException(s"Unexpected error: $x")
              case _ => ()
            }
          }.toList
        }
        .map(_ => ())

    def batchCancels(owner: KeyPair, assetPairs: Iterable[AssetPair]): Future[List[HttpSuccessfulBatchCancel]] = Future.sequence {
      assetPairs.map(dex2.asyncApi.cancelOneOrAllInPairOrdersWithSig(owner, _, System.currentTimeMillis)).toList
    }

    batchCancels(alice, assetPairs)
      .zip(singleCancels(alice, allOrders.filter(_.sender == alice.publicKey)))
      .zip(singleCancels(bob, allOrders.filter(_.sender == bob.publicKey)))
      .zip(batchCancels(bob, assetPairs))
      .futureValue(2.minutes)

    dex1.api.getOrderHistoryByPKWithSig(alice, Some(true)) shouldBe empty
    dex1.api.getOrderHistoryByPKWithSig(bob, Some(true)) shouldBe empty
  }

  private def mkOrders(account: KeyPair, number: Int = placesNumber) =
    Gen.containerOfN[Vector, Order](number, orderGen(matcher, account, assetPairs)).sample.get

  private def state(dexApi: DexApi[Id]) = clean(matcherState(assetPairs, orders, Seq(alice), dexApi))

  // Because we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
  private def clean(state: MatcherState): MatcherState = state.copy(
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )

}
