package com.wavesplatform.it.sync

import java.io.IOException

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{MatcherCommand, MatcherState, SyncMatcherHttpApi, UnexpectedStatusCodeException}
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.tags.DexItKafkaRequired
import com.wavesplatform.it.{Node, _}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalacheck.Gen
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Random, Success}
import scala.util.control.NonFatal

@DexItKafkaRequired
class MultipleMatchersTestSuite extends MatcherSuiteBase {
  private def configOverrides = ConfigFactory.parseString("""waves.dex {
      |  price-assets = ["WAVES"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  private def matcher1NodeConfig = configOverrides.withFallback(Default.head)
  private def matcher2NodeConfig =
    ConfigFactory
      .parseString("""
      |waves {
      |  network.node-name = node11
      |  miner.miner.enable = no
      |}
      |waves.dex.events-queue.kafka.group = 1""".stripMargin)
      .withFallback(matcher1NodeConfig)

  override protected def nodeConfigs: Seq[Config] = Seq(matcher1NodeConfig, matcher2NodeConfig)

  private def matcher1Node = nodes.head
  private def matcher2Node = nodes(1)

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val (issue1, issue2, assetPair1) = issueAssetPair(alice, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, Waves)
  private val assetPair3                   = AssetPair(assetPair1.priceAsset, Waves)
  private val assetPairs                   = Seq(assetPair1, assetPair2, assetPair3)

  // Issue assets by Alice
  private val assetIds = {
    val txs = Seq(issue1, issue2).map(x => matcher1Node.broadcastRequest(x.json()).id -> x)
    txs.map { case (id, info) => nodes.waitForTransaction(id).id -> info }.toMap
  }

  // Share assets with Bob
  {
    val xs = assetIds.map { case (id, info) => node.broadcastTransfer(alice, bob.address, info.quantity / 2, minFee, Some(id), None).id }
    xs.foreach(nodes.waitForTransaction)
  }

  private val aliceOrders = mkOrders(alice)
  private val bobOrders   = mkOrders(alice)
  private val orders      = aliceOrders ++ bobOrders
  private val lastOrder   = orderGen(matcher, alice, assetPairs).sample.get

  "Batch cancel and single cancels simultaneously" in {
    val allOrders =
      Gen.containerOfN[Vector, Order](200, orderGen(matcher, bob, assetPairs, Seq(OrderType.BUY))).sample.get ++
        Gen.containerOfN[Vector, Order](200, orderGen(matcher, alice, assetPairs, Seq(OrderType.BUY))).sample.get

    allOrders.foreach(matcher1Node.placeOrder)
    allOrders.foreach(order => matcher1Node.waitOrderStatus(order.assetPair, order.idStr(), "Accepted"))

    SyncMatcherHttpApi.sync(
      {
        import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

        val asyncNode1 = async(matcher1Node)
        val asyncNode2 = async(matcher2Node)

        def singleCancels(orders: Seq[Order]) = Future.sequence {
          orders.map { order =>
            asyncNode1.cancelOrder(bob, order.assetPair, order.idStr()).transform {
              case Success(_) => Success(())
              case Failure(UnexpectedStatusCodeException(_, _, 400, body)) if (Json.parse(body) \ "status").as[String] == "OrderCancelRejected" =>
                Success(())
              case Failure(cause) => Failure(cause)
            }
          }
        }

        def batchCancels(assetPairs: Set[AssetPair]) = Future.sequence {
          assetPairs.map(asyncNode2.cancelOrdersForPairOnce(bob, _, System.currentTimeMillis()).map { response =>
            if (response.getStatusCode == 503) throw new IOException(s"Unexpected status code: 503")
            else Future.successful(())
          })
        }

        batchCancels(assetPairs.toSet).zip(singleCancels(allOrders))
      },
      2.minute
    )
  }

  "Place, fill and cancel a lot of orders" in {
    val alicePlaces = aliceOrders.map(MatcherCommand.Place(matcher1Node, _))
    val bobPlaces   = bobOrders.map(MatcherCommand.Place(matcher2Node, _))
    val places      = Random.shuffle(alicePlaces ++ bobPlaces)

    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).map(MatcherCommand.Cancel(matcher1Node, alice, _))
    val bobCancels   = (1 to cancelsNumber).map(_ => choose(bobOrders)).map(MatcherCommand.Cancel(matcher2Node, bob, _))
    val cancels      = Random.shuffle(aliceCancels ++ bobCancels)

    executeCommands(places ++ cancels)
    executeCommands(List(MatcherCommand.Place(matcher1Node, lastOrder)))
  }

  "Wait until all requests are processed" in {
    try {
      val offset1 = matcher1Node.waitForStableOffset(10, 100, 200.millis)
      matcher2Node.waitFor[Long](s"Offset is $offset1")(_.getCurrentOffset, _ == offset1, 2.seconds)

      withClue("Last command processed") {
        matcher1Node.waitOrderProcessed(lastOrder.assetPair, lastOrder.idStr())
        matcher2Node.waitOrderProcessed(lastOrder.assetPair, lastOrder.idStr())
      }
    } catch {
      case NonFatal(e) =>
        log.info(s"Last offsets: node1=${matcher1Node.getLastOffset}, node2=${matcher2Node.getLastOffset}")
        throw e
    }
  }

  "States on both matcher should be equal" in {
    val state1 = state(matcher1Node)
    val state2 = state(matcher2Node)
    state1 shouldBe state2
  }

  private def mkOrders(account: KeyPair, number: Int = placesNumber) =
    Gen.containerOfN[Vector, Order](number, orderGen(matcher, account, assetPairs)).sample.get

  private def state(node: Node) = clean(node.matcherState(assetPairs, orders, Seq(alice, bob)))

  // Because we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
  private def clean(state: MatcherState): MatcherState = state.copy(
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )
}
