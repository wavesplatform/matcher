package com.wavesplatform.dex.grpc.integration

import java.net.InetSocketAddress

import com.wavesplatform.dex.grpc.integration.services.BalancesServiceGrpcImpl
import com.wavesplatform.dex.grpc.integration.services.balances._
import com.wavesplatform.dex.grpc.integration.settings.DEXExtensionSettings
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import mouse.any._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.Future

class DEXExtension(context: ExtensionContext) extends Extension with ScorexLogging {

  @volatile
  var server: Server = _

  implicit val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)
  implicit val chosenCase: NameMapper  = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  private def startServer(settings: DEXExtensionSettings): Server = {
    new InetSocketAddress(settings.host, settings.port) |> { bindAddress =>
      NettyServerBuilder
        .forAddress(bindAddress)
        .addService(BalancesServiceGrpc.bindService(new BalancesServiceGrpcImpl(context, settings.balanceChangesBatchLinger), apiScheduler))
        .build
        .start <| (_ => log.info { s"gRPC DEX extension was bound to $bindAddress" })
    }
  }

  override def start(): Unit = context.settings.config.as[DEXExtensionSettings]("waves.dex.grpc.integration") |> { settings =>
    this.server = startServer(settings)
  }

  override def shutdown(): Future[Unit] = {

    log.info("Shutting down gRPC DEX extension")

    if (server != null) {
      server.shutdown()
      Future { server.awaitTermination() }
    } else Future.successful { Unit }
  }
}
