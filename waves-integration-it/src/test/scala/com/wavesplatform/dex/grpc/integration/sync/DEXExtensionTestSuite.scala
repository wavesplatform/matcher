package com.wavesplatform.dex.grpc.integration.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.DEXClient
import com.wavesplatform.dex.grpc.integration.clients.BalancesServiceClient.SpendableBalanceChanges
import com.wavesplatform.it.Docker
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, minFee, someAssetAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observer
import mouse.any._
import org.scalatest.concurrent.Eventually
import org.scalatest.{Assertion, BeforeAndAfterEach}

import scala.concurrent.Future

class DEXExtensionTestSuite extends BaseTransactionSuite with BeforeAndAfterEach with Eventually {

  override protected def nodeConfigs: Seq[Config] = {
    super.nodeConfigs.map(ConfigFactory.parseString("waves.dex.grpc.integration.host = 0.0.0.0").withFallback)
  }

  var balanceChanges           = Map.empty[Address, Map[Asset, Long]]
  val (addressOne, addressTwo) = (getAddress(firstAddress), getAddress(secondAddress))

  val target    = s"localhost:${nodes.head.nodeExternalPort(6887)}"
  val dexClient = new DEXClient(target)

  val eventsObserver: Observer[SpendableBalanceChanges] = new Observer[SpendableBalanceChanges] {
    override def onError(ex: Throwable): Unit                       = Unit
    override def onComplete(): Unit                                 = Unit
    override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balanceChanges = balanceChanges ++ elem; Continue }
  }

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/waves-integration-it:latest",
    tag = getClass.getSimpleName
  )

  def getAddress(addressStr: String): Address = Address.fromString { addressStr }.explicitGet()
  def getAsset(assetStr: String): Asset       = Asset.fromString { Some(assetStr) }

  def assertBalanceChanges(expectedBalanceChanges: Map[Address, Map[Asset, Long]]): Assertion = eventually {
    balanceChanges.filterKeys(expectedBalanceChanges.keys.toSet) shouldBe expectedBalanceChanges
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    dexClient.balancesServiceClient <| { _.requestBalanceChanges() } <| { _.spendableBalanceChanges.subscribe(eventsObserver) }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    balanceChanges = Map.empty[Address, Map[Asset, Long]]
  }

  test("DEX gRPC extension for the Waves node should send balance changes via gRPC") {

    val issuedAssetId =
      sender
        .issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee)
        .id <| nodes.waitForHeightAriseAndTxPresent

    val issuedAsset = getAsset(issuedAssetId)

    assertBalanceChanges {
      Map(
        addressOne -> Map(
          Waves       -> (100.waves - issueFee),
          issuedAsset -> someAssetAmount
        )
      )
    }

    sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId)).id |> nodes.waitForHeightAriseAndTxPresent

    assertBalanceChanges {
      Map(
        addressOne -> Map(
          Waves       -> (100.waves - issueFee - minFee),
          issuedAsset -> 0L
        ),
        addressTwo -> Map(issuedAsset -> someAssetAmount)
      )
    }
  }
}
