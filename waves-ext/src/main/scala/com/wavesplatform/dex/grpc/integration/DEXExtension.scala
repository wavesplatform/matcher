package com.wavesplatform.dex.grpc.integration

import com.typesafe.config.{Config, ConfigException}
import com.wavesplatform.common.state.ByteStr

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.{InternalNettyServerBuilder, NettyServerBuilder}
import monix.execution.{ExecutionModel, Scheduler}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.Future

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  private var server: Server = _

  private var apiService: WavesBlockchainApiGrpcService = _

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  implicit private val apiScheduler: Scheduler = Scheduler(
    ec = context.actorSystem.dispatchers.lookup("akka.actor.waves-dex-grpc-scheduler"),
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  implicit val byteStrValueReader: ValueReader[ByteStr] = (config: Config, path: String) => {
    val str = config.as[String](path)
    ByteStr.decodeBase58(str).fold(th => throw new ConfigException.WrongType(config.origin(), path, "ByteStr in base58", str, th), identity)
  }

  override def start(): Unit = {

    val host: String = context.settings.config.as[String]("waves.dex.grpc.integration.host")
    val port: Int = context.settings.config.as[Int]("waves.dex.grpc.integration.port")
    val allowedBlockchainStateAccounts: Set[ByteStr] =
      context.settings.config.as[Set[ByteStr]]("waves.dex.order-script-validation.allowed-blockchain-state-accounts")

    val bindAddress = new InetSocketAddress(host, port)
    apiService = new WavesBlockchainApiGrpcService(context, allowedBlockchainStateAccounts)

    val builder = NettyServerBuilder
      .forAddress(bindAddress)
      .permitKeepAliveWithoutCalls(true)
      .permitKeepAliveTime(500, TimeUnit.MILLISECONDS)
      .addService(WavesBlockchainApiGrpc.bindService(apiService, apiScheduler))

    InternalNettyServerBuilder.setStatsEnabled(builder, false)
    InternalNettyServerBuilder.setTracingEnabled(builder, false)

    server = builder.build().start()
    log.info(s"gRPC DEX extension was bound to $bindAddress")
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down gRPC DEX extension")
    if (server != null) server.shutdownNow()
    Future.successful(())
  }

}
