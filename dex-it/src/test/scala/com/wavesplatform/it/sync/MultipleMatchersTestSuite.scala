package com.wavesplatform.it.sync

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.docker.base
import com.wavesplatform.dex.it.fp.CanExtract._
import com.wavesplatform.it._
import com.wavesplatform.it.api.{MatcherCommand, MatcherState}
import com.wavesplatform.it.config.DexTestConfig.createAssetPair
import com.wavesplatform.it.tags.DexItKafkaRequired
import org.scalacheck.Gen

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

@DexItKafkaRequired
class MultipleMatchersTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      """waves.dex {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin
    )

  protected lazy val dex2: base.DexContainer = createDex("dex-2")

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val assetPairs = Seq(createAssetPair(eth, wct), ethWavesPair, wctWavesPair)

  private val aliceOrders = mkOrders(alice)
  private val bobOrders   = mkOrders(bob)
  private val orders      = aliceOrders ++ bobOrders
  private val lastOrder   = orderGen(matcher, alice, assetPairs).sample.get

  private var successfulCommandsNumber = 0

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

    val alicePlaces = aliceOrders.map(MatcherCommand.Place(dex1.asyncApi, _))
    val bobPlaces   = bobOrders.map(MatcherCommand.Place(dex2.asyncApi, _))
    val places      = Random.shuffle(alicePlaces ++ bobPlaces)

    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).map(MatcherCommand.Cancel(dex1.asyncApi, alice, _))
    val bobCancels   = (1 to cancelsNumber).map(_ => choose(bobOrders)).map(MatcherCommand.Cancel(dex2.asyncApi, bob, _))
    val cancels      = Random.shuffle(aliceCancels ++ bobCancels)

    successfulCommandsNumber = executeCommands(places ++ cancels)
    successfulCommandsNumber += executeCommands(List(MatcherCommand.Place(dex1.asyncApi, lastOrder)))
  }

  "Wait until all requests are processed" in {
    try {
      val offset1 = dex1.api.waitForCurrentOffset(_ == successfulCommandsNumber - 1) // Index starts from 0
      dex2.api.waitForCurrentOffset(_ == offset1)

      withClue("Last command processed") {
        List(dex1.asyncApi, dex2.asyncApi).foreach(_.waitForOrder(lastOrder)(_.status != OrderStatus.NotFound))
      }
    } catch {
      case NonFatal(e) =>
        log.info(s"Last offsets: node1=${dex1.api.lastOffset}, node2=${dex2.api.lastOffset}")
        throw e
    }
  }

  "States on both matcher should be equal" in {
    val state1 = state(dex1.api)
    val state2 = state(dex2.api)

    state1 should matchTo(state2)
  }

  "Batch cancel and single cancels simultaneously" in {

    dex1.api.cancelAll(alice)
    dex1.api.cancelAll(bob)

    val allOrders =
      (Gen.containerOfN[Vector, Order](150, orderGen(matcher, bob, assetPairs, Seq(OrderType.BUY))).sample.get ++
        Gen.containerOfN[Vector, Order](150, orderGen(matcher, alice, assetPairs, Seq(OrderType.BUY))).sample.get).toSet

    log.info(s"Total orders: ${allOrders.size}")

    allOrders.foreach(dex1.api.place)
    allOrders.foreach(order => dex1.api.waitForOrder(order)(_.status != OrderStatus.NotFound))

    def singleCancels(owner: KeyPair, orders: Iterable[Order]): Future[Iterable[Unit.type]] = Future.sequence {
      orders.map { order =>
        dex1.asyncApi.tryCancel(owner, order).map {
          case Left(x) if x.error != 9437194 => throw new RuntimeException(s"Unexpected error: $x") // OrderCanceled
          case _                             => Unit
        }
      }
    }

    def batchCancels(owner: KeyPair, assetPairs: Iterable[AssetPair]): Future[Iterable[Unit]] = Future.sequence {
      assetPairs.map(toDexExplicitGetOps(dex2.asyncApi).cancelAllByPair(owner, _, System.currentTimeMillis))
    }

    Await.result(
      batchCancels(alice, assetPairs)
        .zip(singleCancels(alice, allOrders.filter(_.sender == alice.publicKey)))
        .zip(singleCancels(bob, allOrders.filter(_.sender == bob.publicKey)))
        .zip(batchCancels(bob, assetPairs)),
      3.minutes
    )

    // TODO implement .waitFor[Seq[OrderbookHistory]]
    Await.result(toDexExplicitGetOps(dex1.asyncApi).orderHistory(alice, Some(true)), 5.seconds) shouldBe empty
    Await.result(toDexExplicitGetOps(dex1.asyncApi).orderHistory(bob, Some(true)), 5.seconds) shouldBe empty
  }

  private def mkOrders(account: KeyPair, number: Int = placesNumber) = {
    Gen.containerOfN[Vector, Order](number, orderGen(matcher, account, assetPairs)).sample.get
  }

  private def state(dexApi: DexApi[Id]) = clean(matcherState(assetPairs, orders, Seq(alice), dexApi))

  // Because we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
  private def clean(state: MatcherState): MatcherState = state.copy(
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )
}
