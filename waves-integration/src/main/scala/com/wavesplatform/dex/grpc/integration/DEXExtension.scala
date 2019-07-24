package com.wavesplatform.dex.grpc.integration

import java.net.InetSocketAddress

import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.dex.grpc.integration.settings.DEXExtensionSettings
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.Future

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  var server: Server = _

  implicit val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

  private def startServer(settings: DEXExtensionSettings): Server = {

    val bindAddress = new InetSocketAddress(settings.host, settings.port)

    val server: Server =
      NettyServerBuilder
        .forAddress(bindAddress)
        .addService(BalancesServiceGrpc.bindService(new BalancesServiceGrpcImpl(context), apiScheduler))
        .build()
        .start()

    log.info(s"gRPC DEX extension was bound to $bindAddress")

    server
  }

  override def start(): Unit = {
    val settings = context.settings.config.as[DEXExtensionSettings]("waves.dex.grpc.integration")
    this.server = startServer(settings)
  }

  override def shutdown(): Future[Unit] = {

    log.debug("Shutting down gRPC DEX extension")

    if (server != null) {
      server.shutdown()
      Future { server.awaitTermination() }
    } else Future.successful { Unit }
  }
}
