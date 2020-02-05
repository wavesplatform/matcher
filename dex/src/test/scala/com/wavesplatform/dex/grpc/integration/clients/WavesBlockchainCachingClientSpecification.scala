package com.wavesplatform.dex.grpc.integration.clients

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * This test is placed here (not in `waves-integration`) because of sub-project dependencies problem.
  * Module `dex-test-common` depends on `waves-integration`, therefore this test could not use DoubleOps and WavesFeeConstants from `dex-test-common`.
  * After discussion it was decided to place this test here in order to avoid global sub-projects refactoring
  */
class WavesBlockchainCachingClientSpecification extends AnyWordSpecLike with Matchers with MatcherSpecBase {

  "WavesBlockchainCachingClient" should {

    "cache balances after calling of allBalanceAssetsSpendableBalance method" in {

      val senderBalance: Map[Asset, Long] = Map(
        Waves -> 100.waves,
        eth   -> 50.eth,
        btc   -> 10.btc,
      )

      val balancesMap: Map[Address, Map[Asset, Long]] = Map(
        senderKeyPair.toAddress -> senderBalance
      )

      val asyncClient =
        new WavesBlockchainClient[Future] {

          override def spendableBalanceChanges: Observable[SpendableBalanceChanges]   = ???
          override def spendableBalance(address: Address, asset: Asset): Future[Long] = ???

          override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] = Future.successful { balancesMap(address) }

          override def isFeatureActivated(id: Short): Future[Boolean]                                           = ???
          override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]]        = ???
          override def hasScript(asset: Asset.IssuedAsset): Future[Boolean]                                     = ???
          override def runScript(asset: Asset.IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = ???
          override def hasScript(address: Address): Future[Boolean]                                             = ???
          override def runScript(address: Address, input: Order): Future[RunScriptResult]                       = ???
          override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]                           = ???
          override def broadcastTx(tx: ExchangeTransaction): Future[Boolean]                                    = ???
          override def forgedOrder(orderId: ByteStr): Future[Boolean]                                           = ???
          override def close(): Future[Unit]                                                                    = ???
        }

      val cachingClient =
        new WavesBlockchainCachingClient(asyncClient, 1.second, monix.execution.Scheduler.global)(scala.concurrent.ExecutionContext.global)

      withClue("allAssetsSpendableBalance updates balance cache\n") {
        awaitResult { cachingClient.allAssetsSpendableBalance(senderKeyPair) } should matchTo(senderBalance)
      }

      withClue("With cached balance method spendableBalance should not produce error\n") {
        awaitResult { cachingClient.spendableBalance(senderKeyPair, Waves) } shouldBe 100.waves
        awaitResult { cachingClient.spendableBalance(senderKeyPair, eth) } shouldBe 50.eth
        awaitResult { cachingClient.spendableBalance(senderKeyPair, btc) } shouldBe 10.btc
      }

      withClue("USD balance is not cached, therefore an error should be thrown\n") {
        a[com.google.common.util.concurrent.ExecutionError] should be thrownBy awaitResult { cachingClient.spendableBalance(senderKeyPair, usd) }
      }
    }
  }
}
