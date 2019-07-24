package com.wavesplatform.dex.grpc.integration.sync

import com.google.protobuf.empty.Empty
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.dto.{BalanceChangesResponse => VBalanceChangesResponse}
import com.wavesplatform.dex.grpc.integration.protobuf.BalancesServiceConversions
import com.wavesplatform.dex.grpc.integration.protobuf.Implicits._
import com.wavesplatform.dex.grpc.integration.services.BalancesServiceGrpc.BalancesServiceStub
import com.wavesplatform.dex.grpc.integration.services.{BalanceChangesRequest, BalanceChangesResponse, BalancesServiceGrpc}
import com.wavesplatform.it.Docker
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, minFee, someAssetAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import io.grpc.stub.StreamObserver
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import mouse.any._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class DEXExtensionTestSuite extends BaseTransactionSuite {

  val balanceChanges = scala.collection.mutable.Map.empty[Address, Seq[(Asset, Long)]]

  override protected def nodeConfigs: Seq[Config] = {
    super.nodeConfigs.map(ConfigFactory.parseString("waves.dex.grpc.integration.host = 0.0.0.0").withFallback)
  }

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/waves-integration-it:latest",
    tag = getClass.getSimpleName
  )

  lazy val channel: ManagedChannel     = ManagedChannelBuilder.forAddress("localhost", nodes.head.nodeExternalPort(6887)).usePlaintext().build
  lazy val client: BalancesServiceStub = BalancesServiceGrpc.stub(channel)

  lazy val responseObserver: StreamObserver[BalanceChangesResponse] = new StreamObserver[BalanceChangesResponse] {
    def onError(t: Throwable): Unit = Unit
    def onCompleted(): Unit         = Unit
    def onNext(value: BalanceChangesResponse): Unit =
      BalancesServiceConversions.vanilla(value) |> {
        case VBalanceChangesResponse(address, asset, balance) =>
          balanceChanges.update(address, balanceChanges.getOrElse(address, Seq.empty) :+ (asset -> balance))
      }
  }

  lazy val requestObserver: StreamObserver[BalanceChangesRequest] = client.getBalanceChanges(responseObserver)

  private def getAddress(addressStr: String): Address = Address.fromString(addressStr).explicitGet()
  private def getAsset(assetStr: String): Asset       = Asset.fromString(Some(assetStr))

  private def getCurrentObservableBalances: Future[Map[String, Set[Asset]]] = {
    client.getCurrentObservableBalances { Empty() }.map { resp =>
      resp.observableBalancesMap.map { case (addressStr, assets) => addressStr -> assets.assets.map(_.toVanillaAsset).toSet }
    }
  }

  test("DEX gRPC extension for the Waves node should be tunable and send balance changes via gRPC") {

    // scenario:
    // 1. check that initial observable balances on the node (server) side are empty
    // 2. add subscriptions for first and second addresses by Waves and check that now observable balances are not empty
    // 3. first address issues asset
    // 4. add subscriptions for first and second addresses by issued asset and check that now observable balances are correct
    // 5. first address sends issued asset to the second address
    // 6. check balance changes for both addresses

    val addressOne = getAddress(firstAddress)
    val addressTwo = getAddress(secondAddress)

    withClue("There aren't observable balances in the beginning\n") {
      Await.result(getCurrentObservableBalances, 30.seconds) shouldBe empty
    }

    Thread.sleep(1000) // overcome messages buffering

    withClue("Tune DEX extension output stream first time (add subscriptions for two addresses by Waves)\n") {

      Seq(addressOne, addressTwo) foreach { address =>
        requestObserver.onNext { BalanceChangesRequest(address.toPBAddress, Waves.toPBAsset) }
      }

      Thread.sleep(1000) // overcome messages buffering

      Await.result(getCurrentObservableBalances, 30.seconds) shouldBe
        Map(
          firstAddress  -> Set(Waves),
          secondAddress -> Set(Waves)
        )
    }

    val issuedAssetId =
      sender
        .issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee)
        .id <| nodes.waitForHeightAriseAndTxPresent

    val issuedAsset = getAsset(issuedAssetId)

    withClue("Tune DEX extension output stream second time (add subscriptions for the first and second addresses by issued asset)\n") {

      Seq(addressOne, addressTwo) foreach { address =>
        requestObserver.onNext { BalanceChangesRequest(address.toPBAddress, issuedAsset.toPBAsset) }
      }

      Thread.sleep(1000) // overcome messages buffering

      Await.result(getCurrentObservableBalances, 30.seconds) shouldBe
        Map(
          firstAddress  -> Set(Waves, issuedAsset),
          secondAddress -> Set(Waves, issuedAsset)
        )
    }

    sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId)).id |> nodes.waitForHeightAriseAndTxPresent

    withClue("Check balance changes for both addresses. NOTE: there are duplications due to (seemingly) UTX pool implementation\n") {

      val firstAddressBalanceChanges =
        Set(
          Waves       -> 100.waves, // default balance
          Waves       -> (100.waves - issueFee),
          Waves       -> (100.waves - issueFee - minFee),
          issuedAsset -> someAssetAmount,
          issuedAsset -> 0
        )

      val secondAddressBalanceChanges =
        Set(
          Waves       -> 100.waves,
          issuedAsset -> someAssetAmount
        )

      balanceChanges.mapValues(_.toSet) shouldBe
        Map(
          getAddress(firstAddress)  -> firstAddressBalanceChanges,
          getAddress(secondAddress) -> secondAddressBalanceChanges
        )
    }
  }
}
