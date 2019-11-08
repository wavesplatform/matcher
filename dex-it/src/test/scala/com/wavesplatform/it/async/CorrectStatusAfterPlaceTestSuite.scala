package com.wavesplatform.it.async

import cats.instances.future._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it._
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.transfer.MassTransferTransaction

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class CorrectStatusAfterPlaceTestSuite extends MatcherSuiteBase {

  private val issuer                                                    = alice
  private val IssueResults(issueAsset1Tx, issuedAsset1Id, issuedAsset1) = mkIssueExtended(issuer, "asset1", Long.MaxValue, decimals = 0)
  private val IssueResults(issueAsset2Tx, issuedAsset2Id, issuedAsset2) = mkIssueExtended(issuer, "asset2", Long.MaxValue, decimals = 0)
  private val issueAssetTxs                                             = List(issueAsset1Tx, issueAsset2Tx)

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = ["$issuedAsset1Id", "$issuedAsset2Id"]
       |  rest-order-limit = 100
       |  events-queue {
       |    local {
       |      polling-interval = 1s
       |      max-elements-per-poll = 100
       |    }
       |    kafka.consumer.buffer-size = 100
       |  }
       |}
       |akka.kafka.consumer.poll-interval = 1s""".stripMargin
  )

  private val pairs =
    Seq(
      AssetPair(Waves, issuedAsset1),
      AssetPair(Waves, issuedAsset2),
      AssetPair(issuedAsset2, issuedAsset1),
    )

  private val traders: Seq[KeyPair] = (1 to 10).map(_ => KeyPair(Random.nextString(20).getBytes))

  override protected def beforeAll(): Unit = {
    val sendAmount = Long.MaxValue / (traders.size + 1)

    val transferWavesTx =
      mkMassTransfer(
        bob,
        Waves,
        traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, 100.waves))(collection.breakOut)
      )

    startAndWait(wavesNode1Container(), wavesNode1Api)
    wavesNode1Api.broadcast(transferWavesTx)
    broadcastAndAwait(issueAssetTxs: _*)

    val transferAssetsTxs = issueAssetTxs.map { issueTx =>
      mkMassTransfer(issuer,
                     IssuedAsset(issueTx.id.value),
                     traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, sendAmount))(collection.breakOut))
    }

    broadcastAndAwait(transferAssetsTxs: _*)
    wavesNode1Api.waitForTransaction(transferWavesTx)

    startAndWait(dex1Container(), dex1Api)
  }

  "place orders and check their statuses" in {

    val ts = System.currentTimeMillis()

    val orders = for {
      account <- traders
      pair    <- pairs
      i       <- 1 to 60
    } yield mkOrder(account, pair, OrderType.SELL, 100000L, 10000L, ts = ts + i)

    Await
      .result(
        Future.traverse { orders.grouped(orders.size / 5) }(requests),
        10.minutes
      )
      .flatten
      .foreach {
        case (id, status) => withClue(s"$id")(status should not be OrderStatus.NotFound)
      }
  }

  private def request(order: Order): Future[(Order.Id, OrderStatus)] = {
    for {
      _      <- dex1AsyncApi.tryPlace(order).recover { case x => log.error("Some error with order placement occurred:", x) }
      status <- dex1AsyncApi.orderStatus(order)
    } yield (order.id(), status.status)
  }

  private def requests(orders: Seq[Order]): Future[Seq[(Order.Id, OrderStatus)]] = Future.traverse(orders)(request)
}
