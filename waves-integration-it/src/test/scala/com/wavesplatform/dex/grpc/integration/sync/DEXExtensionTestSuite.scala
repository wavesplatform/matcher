package com.wavesplatform.dex.grpc.integration.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.DEXClient
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

  val balanceChanges           = scala.collection.mutable.Map.empty[Address, Seq[(Asset, Long)]]
  val (addressOne, addressTwo) = (getAddress(firstAddress), getAddress(secondAddress))

  val target    = s"localhost:${nodes.head.nodeExternalPort(6887)}"
  val dexClient = new DEXClient(target)

  val eventsObserver: Observer[(Address, Asset, Long)] = new Observer[(Address, Asset, Long)] {
    override def onError(ex: Throwable): Unit = Unit
    override def onComplete(): Unit           = Unit
    override def onNext(elem: (Address, Asset, Long)): Future[Ack] = elem |> {
      case (address, asset, balance) =>
        balanceChanges.update(address, balanceChanges.getOrElse(address, Seq.empty) :+ (asset -> balance))
        Continue
    }
  }

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/waves-integration-it:latest",
    tag = getClass.getSimpleName
  )

  def getAddress(addressStr: String): Address = Address.fromString { addressStr }.explicitGet()
  def getAsset(assetStr: String): Asset       = Asset.fromString { Some(assetStr) }

  def assertBalanceChanges(address: Address)(expectedBalanceChanges: Set[(Asset, Long)]): Assertion = eventually {
    balanceChanges.get(address).map(_.toSet) shouldBe Some(expectedBalanceChanges)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    dexClient.balanceServiceClient <| { _.requestBalanceChanges() } <| { _.spendableBalanceChanges.subscribe(eventsObserver) }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    balanceChanges.clear()
  }

  test("DEX gRPC extension for the Waves node should send balance changes via gRPC") {

    val issuedAssetId =
      sender
        .issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee)
        .id <| nodes.waitForHeightAriseAndTxPresent

    val issuedAsset = getAsset(issuedAssetId)

    sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId)).id |> nodes.waitForHeightAriseAndTxPresent

    assertBalanceChanges(addressOne) {
      Set(
        Waves       -> 100.waves, // default balance
        Waves       -> (100.waves - issueFee), // first address issued asset
        Waves       -> (100.waves - issueFee - minFee), // first address sent asset to the second address
        issuedAsset -> someAssetAmount, // initial asset amount
        issuedAsset -> 0L // balance after transfer
      )
    }

    assertBalanceChanges(addressTwo) {
      Set(
        Waves       -> 100.waves, // default balance
        issuedAsset -> someAssetAmount // balance after transfer
      )
    }
  }
}
