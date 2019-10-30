package com.wavesplatform.dex.grpc.integration.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.{DEXClient, ItTestSuiteBase}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import mouse.any._
import org.scalatest.{Assertion, BeforeAndAfterEach}

import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class WavesBlockchainAsyncClientTestSuite extends ItTestSuiteBase with BeforeAndAfterEach {

  private val monixScheduler = Scheduler.singleThread("test")

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(
    timeout = 1.minute,
    interval = 1.second
  )

  override val suiteInitialWavesNodeConfig: Config = ConfigFactory.parseString("waves.dex.grpc.integration.host = 0.0.0.0")

  private var balanceChanges = Map.empty[Address, Map[Asset, Long]]

  private val eventsObserver: Observer[SpendableBalanceChanges] = new Observer[SpendableBalanceChanges] {
    override def onError(ex: Throwable): Unit                       = Unit
    override def onComplete(): Unit                                 = Unit
    override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balanceChanges ++= elem; Continue }
  }

  private def assertBalanceChanges(expectedBalanceChanges: Map[Address, Map[Asset, Long]]): Assertion = eventually {
    val actual = balanceChanges.filterKeys(expectedBalanceChanges.keys.toSet)
    log.trace(s"Compare:\nactual: $actual\nexpected: $expectedBalanceChanges")
    actual should contain theSameElementsAs expectedBalanceChanges
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
    new DEXClient(wavesNode1GrpcApiTarget, 100.milliseconds, monixScheduler, executionContext).wavesBlockchainAsyncClient
      .unsafeTap { _.requestBalanceChanges() }
      .unsafeTap { _.spendableBalanceChanges.subscribe(eventsObserver)(monixScheduler) }
  }

  "WavesBlockchainAsyncClient should send balance changes via gRPC" in {

    val aliceInitialBalance = wavesNode1Api.balance(alice, Waves)

    val issueAssetTx = mkIssue(alice, "name", someAssetAmount, 2)
    val issuedAsset  = IssuedAsset(issueAssetTx.id.value)

    balanceChanges = Map.empty[Address, Map[Asset, Long]]
    broadcastAndAwait(issueAssetTx)

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee),
          issuedAsset -> someAssetAmount
        )
      )
    }

    balanceChanges = Map.empty[Address, Map[Asset, Long]]
    broadcastAndAwait(mkTransfer(alice, bob, someAssetAmount, issuedAsset))

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee - minFee),
          issuedAsset -> 0L
        ),
        bob.toAddress -> Map(
          issuedAsset -> someAssetAmount
        )
      )
    }
  }
}
