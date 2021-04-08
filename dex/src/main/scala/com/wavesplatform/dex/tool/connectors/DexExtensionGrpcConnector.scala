package com.wavesplatform.dex.tool.connectors

import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import ch.qos.logback.classic.{Level, Logger}
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.clients.combined.{CombinedStream, CombinedWavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings.ChannelOptionsSettings
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import monix.execution.Scheduler.Implicits.{global => monixScheduler}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, Future}
import scala.util.Try

case class DexExtensionGrpcConnector private (target: String, grpcAsyncClient: WavesBlockchainClient) extends Connector {

  import DexExtensionGrpcConnector._

  private def sync[A](f: Awaitable[A]): A = Await.result(f, requestTimeout)

  private def getDetailedBalance(asset: Asset, balance: Long): Future[(Asset, (BriefAssetDescription, Long))] = asset match {
    case Waves => Future.successful(asset -> (BriefAssetDescription.wavesDescription -> balance))
    case ia: IssuedAsset => grpcAsyncClient.assetDescription(ia).map(maybeDesc => ia -> (maybeDesc.get -> balance))
  }

  def matcherBalanceAsync(address: Address): Future[DetailedBalance] =
    for {
      balances <- grpcAsyncClient.fullBalancesSnapshot(address, Set.empty).map(_.regular)
      balancesWithDescription <- balances.toList.traverse { case (a, b) => getDetailedBalance(a, b) }
    } yield balancesWithDescription.toMap

  def matcherBalanceSync(address: Address): DetailedBalance = sync(matcherBalanceAsync(address))

  override def close(): Unit = Await.result(grpcAsyncClient.close(), 3.seconds)
}

object DexExtensionGrpcConnector {

  val requestTimeout: FiniteDuration = 10.seconds

  type DetailedBalance = Map[Asset, (BriefAssetDescription, Long)]

  def create(matcherPublicKey: PublicKey, target: String, blockchainUpdatesTarget: String): ErrorOr[DexExtensionGrpcConnector] =
    Try {
      LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.OFF)
      val grpcSettings = GrpcClientSettings(target, 5, 5, true, 2.seconds, 5.seconds, 1.minute, ChannelOptionsSettings(5.seconds), 5.minutes)
      val blockchainUpdatesGrpcSettings =
        GrpcClientSettings(blockchainUpdatesTarget, 5, 5, true, 2.seconds, 5.seconds, 1.minute, ChannelOptionsSettings(5.seconds), 5.minutes)
      val clientSettings = WavesBlockchainClientSettings(
        grpcSettings,
        blockchainUpdatesGrpcSettings,
        100.milliseconds,
        100,
        CombinedWavesBlockchainClient.Settings(100, 5, CombinedStream.Settings(1.second), SynchronizedPessimisticPortfolios.Settings(100))
      )
      CombinedWavesBlockchainClient(clientSettings, matcherPublicKey, monixScheduler, executionContext)
    }.toEither
      .bimap(ex => s"Cannot establish gRPC connection to DEX Extension! $ex", client => DexExtensionGrpcConnector(target, client))

}
