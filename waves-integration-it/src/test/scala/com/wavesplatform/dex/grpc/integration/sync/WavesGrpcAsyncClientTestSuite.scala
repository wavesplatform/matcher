package com.wavesplatform.dex.grpc.integration.sync

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBalancesClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.sync.WavesGrpcAsyncClientTestSuite._
import com.wavesplatform.dex.grpc.integration.{DEXClient, ItTestSuiteBase}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.wallet.Wallet
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observer
import mouse.any._
import org.scalatest.concurrent.Eventually
import org.scalatest.{Assertion, BeforeAndAfterEach}

import scala.collection.JavaConverters._
import scala.concurrent.Future

class WavesGrpcAsyncClientTestSuite extends ItTestSuiteBase with BeforeAndAfterEach with Eventually {

  override protected def nodeConfigs: Seq[Config] =
    super.nodeConfigs.map(ConfigFactory.parseString("waves.dex.grpc.integration.host = 0.0.0.0").withFallback)

  private var balanceChanges = Map.empty[Address, Map[Asset, Long]]

  private val eventsObserver: Observer[SpendableBalanceChanges] = new Observer[SpendableBalanceChanges] {
    override def onError(ex: Throwable): Unit                       = Unit
    override def onComplete(): Unit                                 = Unit
    override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balanceChanges = balanceChanges ++ elem; Continue }
  }

  private def assertBalanceChanges(expectedBalanceChanges: Map[Address, Map[Asset, Long]]): Assertion = eventually {
    balanceChanges.filterKeys(expectedBalanceChanges.keys.toSet) shouldBe expectedBalanceChanges
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    val target = s"${node.networkAddress.getHostString}:${nodes.head.nodeExternalPort(6887)}"
    new DEXClient(target).wavesBalancesAsyncClient.unsafeTap { _.requestBalanceChanges() }.unsafeTap {
      _.spendableBalanceChanges.subscribe(eventsObserver)
    }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    balanceChanges = Map.empty[Address, Map[Asset, Long]]
  }

  "WavesBalancesApiGrpcServer should send balance changes via gRPC" in {

    val aliceInitialBalance = node.balanceDetails(alice.toAddress.stringRepr).available
    val bobInitialBalance   = node.balanceDetails(bob.toAddress.stringRepr).available

    val issueAssetTx = mkIssue(alice, "name", someAssetAmount, 2)
    val issuedAsset  = IssuedAsset(issueAssetTx.id.value)

    node.broadcastRequest(issueAssetTx.json.value).id
    nodes.waitForTransaction(issueAssetTx.id.value.toString)

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee),
          issuedAsset -> someAssetAmount
        )
      )
    }

    val transferTx = mkTransfer(alice, bob, someAssetAmount, issuedAsset)
    node.broadcastRequest(transferTx.json.value)
    nodes.waitForTransaction(transferTx.id.value.toString)

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee - minFee),
          issuedAsset -> 0L
        ),
        bob.toAddress -> Map(
          Waves       -> bobInitialBalance,
          issuedAsset -> someAssetAmount
        )
      )
    }
  }
}

object WavesGrpcAsyncClientTestSuite {
  private val accounts: Map[String, KeyPair] = {
    val config           = ConfigFactory.parseResources("genesis.conf")
    val distributionsKey = "genesis-generator.distributions"
    val distributions    = config.getObject(distributionsKey)

    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix   = s"$distributionsKey.$accountName"
        val seedText = config.getString(s"$prefix.seed-text")
        val nonce    = config.getInt(s"$prefix.nonce")
        accountName -> Wallet.generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

  private val alice: KeyPair = accounts("alice")
  private val bob: KeyPair   = accounts("bob")
}
