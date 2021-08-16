package com.wavesplatform.it.async

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.Implicits.durationToScalatestTimeout
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transactions.mass.Transfer

import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.concurrent.duration._

// leave default Patience config with timeout = 30 seconds
// but if there will be timeout errors, set it up to 1-2 minutes,
// because it was Duration.Inf timeout before
class CorrectStatusAfterPlaceTestSuite extends MatcherSuiteBase {

  private val issuer = alice
  private val now = System.currentTimeMillis()

  implicit override def patienceConfig = PatienceConfig(3.minutes, 1.second)

  private val IssueResults(issueAsset1Tx, issuedAsset1Id, issuedAsset1) =
    mkIssueExtended(issuer, "asset1", Long.MaxValue, decimals = 0, timestamp = now)

  private val IssueResults(issueAsset2Tx, issuedAsset2Id, issuedAsset2) =
    mkIssueExtended(issuer, "asset2", Long.MaxValue, decimals = 0, timestamp = now + 1)

  private val issueAssetTxs = List(issueAsset1Tx, issueAsset2Tx)

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = ["$issuedAsset1Id", "$issuedAsset2Id"]
       |  rest-order-limit = 100
       |  events-queue {
       |    local {
       |      polling-interval = 1s
       |      max-elements-per-poll = 200
       |    }
       |    kafka.consumer {
       |      fetch-max-duration = 1s
       |      max-buffer-size = 200
       |    }
       |  }
       |}""".stripMargin
  )

  private val pairs =
    Seq(
      AssetPair(Waves, issuedAsset1),
      AssetPair(Waves, issuedAsset2),
      AssetPair(issuedAsset2, issuedAsset1)
    )

  private val traders: Seq[KeyPair] = (1 to 10).map(i => KeyPair(s"trader-$i".getBytes(StandardCharsets.UTF_8)))

  override protected def beforeAll(): Unit = {
    val sendAmount = Long.MaxValue / (traders.size + 1)

    val transferWavesTx =
      mkMassTransfer(
        bob,
        Waves,
        traders.map(x => new Transfer(x.toAddress, 100.waves)).to(List)
      )

    wavesNode1.start()
    wavesNode1.api.broadcast(transferWavesTx)
    broadcastAndAwait(issueAssetTxs: _*)

    val transferAssetsTxs = issueAssetTxs.map { issueTx =>
      mkMassTransfer(issuer, IssuedAsset(issueTx.id()), traders.map(x => new Transfer(x.toAddress, sendAmount)).to(List))
    }

    broadcastAndAwait(transferAssetsTxs: _*)
    wavesNode1.api.waitForTransaction(transferWavesTx)

    dex1.start()
  }

  "place orders and check their statuses" in {
    val ts = System.currentTimeMillis()
    val accountOrderInPair = 60

    val orders = for {
      account <- traders
      pair <- pairs
      i <- 1 to accountOrderInPair
    } yield mkOrder(account, pair, OrderType.SELL, 100000L, 10000L, ts = ts + i)

    val future = for {
      r <- Future.traverse(orders.grouped(orders.size / 5))(requests)
      _ <- {
        val totalSent = r.map(_.count(_._3)).sum
        dex1.asyncApi.waitForCurrentOffset(_ == totalSent - 1)
      }
    } yield r

    // There are 1800 orders and the contention during running tests in parallel on CI.
    // So we specified the huge timeout for this job.
    future.futureValue(10.minutes)
      .flatten
      .foreach {
        case (id, status, sent) => if (sent) withClue(s"$id")(status should not be Status.NotFound)
      }
  }

  private def request(order: Order): Future[(Order.Id, HttpOrderStatus.Status, Boolean)] =
    for {
      // TODO happens rarely, try to remove after migration to new akka-http
      sent <- dex1.asyncTryApi.place(order).map(_ => true).recover { case x => log.error("Some error with order placement occurred:", x); false }
      status <- dex1.asyncApi.orderStatusByAssetPairAndId(order)
    } yield (order.id(), status.status, sent)

  private def requests(orders: Seq[Order]): Future[Seq[(Id, Status, Boolean)]] = Future.traverse(orders)(request)
}
