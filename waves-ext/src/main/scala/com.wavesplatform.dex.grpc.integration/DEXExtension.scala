package com.wavesplatform.dex.grpc.integration

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.dex.grpc.integration.settings.DEXExtensionSettings
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.Future

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  private var server: Server = _

  implicit val chosenCase: NameMapper          = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit private val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

  private def startServer(settings: DEXExtensionSettings): Server = {
    val bindAddress = new InetSocketAddress(settings.host, settings.port)
    val server = NettyServerBuilder
      .forAddress(bindAddress)
      .permitKeepAliveWithoutCalls(true)
      .permitKeepAliveTime(500, TimeUnit.MILLISECONDS)
      .addService(WavesBlockchainApiGrpc.bindService(new WavesBlockchainApiGrpcService(context, settings.balanceChangesBatchLinger), apiScheduler))
      .build()
      .start()

    log.info(s"gRPC DEX extension was bound to $bindAddress")
    server
  }

  override def start(): Unit = server = startServer(context.settings.config.as[DEXExtensionSettings]("waves.dex.grpc.integration"))

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down gRPC DEX extension")
    if (server != null) server.shutdownNow()
    Future.successful { Unit }
  }
}
