package com.wavesplatform.dex.grpc.integration

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.{ExecutionModel, Scheduler}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  private var server: Server                            = _
  private var apiService: WavesBlockchainApiGrpcService = _

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit private val apiScheduler: Scheduler = Scheduler(
    ec = context.actorSystem.dispatchers.lookup("akka.actor.waves-dex-grpc-scheduler"),
    executionModel = ExecutionModel.AlwaysAsyncExecution
  )

  override def start(): Unit = {

    val host: String                  = context.settings.config.as[String]("waves.dex.grpc.integration.host")
    val port: Int                     = context.settings.config.as[Int]("waves.dex.grpc.integration.port")
    val batchLingerMs: FiniteDuration = context.settings.config.as[FiniteDuration]("waves.dex.grpc.integration.balance-changes-batch-linger")

    val bindAddress = new InetSocketAddress(host, port)
    apiService = new WavesBlockchainApiGrpcService(context, batchLingerMs)

    server = NettyServerBuilder
      .forAddress(bindAddress)
      .permitKeepAliveWithoutCalls(true)
      .permitKeepAliveTime(500, TimeUnit.MILLISECONDS)
      .addService(WavesBlockchainApiGrpc.bindService(apiService, apiScheduler))
      .build()
      .start()

    log.info(s"gRPC DEX extension was bound to $bindAddress")
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down gRPC DEX extension")
    if (server != null) server.shutdownNow()
    Future.successful(())
  }
}
