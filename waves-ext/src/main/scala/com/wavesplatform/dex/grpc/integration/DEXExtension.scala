package com.wavesplatform.dex.grpc.integration

import java.net.InetSocketAddress
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.{InternalNettyServerBuilder, NettyServerBuilder}
import monix.execution.{ExecutionModel, Scheduler}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  private var server: Server = _

  private var apiService: WavesBlockchainApiGrpcService = _

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  implicit private val apiScheduler: Scheduler = Scheduler(
    ec = context.actorSystem.dispatchers.lookup("akka.actor.waves-dex-grpc-scheduler"),
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  override def start(): Unit = {

    val host: String = context.settings.config.as[String]("waves.dex.grpc.integration.host")
    val port: Int = context.settings.config.as[Int]("waves.dex.grpc.integration.port")
    val permitKeepAliveTime: FiniteDuration = context.settings.config.as[FiniteDuration]("waves.blockchain-updates.min-keep-alive")

    val bindAddress = new InetSocketAddress(host, port)
    apiService = new WavesBlockchainApiGrpcService(context)

    val builder = NettyServerBuilder
      .forAddress(bindAddress)
      .permitKeepAliveWithoutCalls(true)
      .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
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
