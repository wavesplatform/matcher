package com.wavesplatform.dex.grpc.integration

import com.typesafe.config.{Config, ConfigException}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.{InternalNettyServerBuilder, NettyServerBuilder}
import monix.execution.{ExecutionModel, Scheduler}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import java.io._
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

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
    decodeBase58(str, config)
  }

  override def start(): Unit = {

    val host: String = context.settings.config.as[String]("waves.dex.grpc.integration.host")
    val port: Int = context.settings.config.as[Int]("waves.dex.grpc.integration.port")
    val allowedBlockchainStateAccounts: Set[ByteStr] =
      context.settings.config.as[Set[ByteStr]]("waves.dex.order-script-validation.allowed-blockchain-state-accounts")
    val lpAccountsFilePath: String = context.settings.config.as[String]("waves.dex.lp-accounts.file-path")
    val lpAccounts: Set[ByteStr] = lpAccountsFromPath(lpAccountsFilePath, context.settings.config)

    apiService = new WavesBlockchainApiGrpcService(context, allowedBlockchainStateAccounts, lpAccounts)

    val bindAddress = new InetSocketAddress(host, port)

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

  private def lpAccountsFromPath(filePath: String, config: Config): Set[ByteStr] =
    try {
      val file = new File(filePath)
      val stream = new FileInputStream(file)
      val streamReader = new InputStreamReader(stream)
      val bufferedReader = new BufferedReader(streamReader)
      bufferedReader.lines
        .filter(_.nonEmpty)
        .map[ByteStr](decodeBase58(_, config))
        .iterator
        .asScala
        .toSet
    } catch {
      case ce: ConfigException => throw ce
      case e: Throwable => throw new ConfigException.BadPath(filePath, s"couldn't access the file $filePath", e)
    }

  private def decodeBase58(str: String, config: Config): ByteStr =
    ByteStr.decodeBase58(str).fold(
      th => throw new ConfigException.WrongType(config.origin(), s"string $str cannot be decoded in base58", th),
      identity
    )

}
