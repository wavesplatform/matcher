package com.wavesplatform.dex.tool.connectors

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import ch.qos.logback.classic.{Level, Logger}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.grpc.integration.WavesBlockchainClientBuilder
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings.ChannelOptionsSettings
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import com.wavesplatform.dex.tool.connectors.SuperConnector.wrapByLogs
import monix.execution.Scheduler.Implicits.{global => monixScheduler}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

private[tool] case class DexGrpcConnector(target: String, matcherKeyPair: KeyPair) extends Connector {

  LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.OFF)

  private val grpcAsyncClient: WavesBlockchainClient[Future] = wrapByLogs("Establishing gRPC connection to DEX extension... ") {
    val grpcSettings   = GrpcClientSettings(target, 5, 5, true, 2.seconds, 5.seconds, 1.minute, ChannelOptionsSettings(5.seconds))
    val clientSettings = WavesBlockchainClientSettings(grpcSettings, 100.milliseconds, 100)
    WavesBlockchainClientBuilder.async(clientSettings, monixScheduler, executionContext)
  }

  private def getDetailedBalance(asset: Asset, balance: Long): Future[(Asset, (BriefAssetDescription, Long))] = asset match {
    case Waves           => Future.successful(asset -> (BriefAssetDescription.wavesDescription -> balance))
    case ia: IssuedAsset => grpcAsyncClient.assetDescription(ia).map(maybeDesc => ia -> (maybeDesc.get -> balance))
  }

  def matcherBalance: Map[Asset, (BriefAssetDescription, Long)] = Await.result(
    for {
      balances                <- grpcAsyncClient.allAssetsSpendableBalance(matcherKeyPair.toAddress)
      balancesWithDescription <- balances.toList.traverse { case (a, b) => getDetailedBalance(a, b) }
    } yield balancesWithDescription.toMap,
    10.seconds
  )

  override def close(): Unit = grpcAsyncClient.close()
}
