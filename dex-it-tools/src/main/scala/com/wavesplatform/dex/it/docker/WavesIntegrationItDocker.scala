package com.wavesplatform.dex.it.docker

import java.io.FileNotFoundException
import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}

import scala.io.Source
import scala.util.Try

object WavesIntegrationItDocker {

  private val wavesBaseConfFileName = "waves-base.conf"
  private val wavesNodeImage        = "com.wavesplatform/waves-integration-it:latest"

  private def getRawConfig(fileName: String): String = {
    Try(Source fromResource fileName).getOrElse { throw new FileNotFoundException(s"Resource '$fileName'") }.mkString
  }

  def createContainer(
      client: Docker)(name: String, runConfig: Config, initialSuiteConfig: Config, netAlias: Option[String] = None): WavesNodeContainer = {

    val number   = getNumber(name)
    val basePath = "/opt/waves"

    val id =
      client.create(
        number = number,
        name = name,
        imageName = wavesNodeImage,
        env = Map(
          "WAVES_NODE_CONFIGPATH" -> s"$basePath/$name.conf",
          "WAVES_OPTS"            -> s" -Xmx1024M -Dlogback.configurationFile=$basePath/logback.xml"
        ),
        netAlias = netAlias
      )

    val rawBaseConfig = getRawConfig(s"nodes/$wavesBaseConfFileName")
    val baseConfig    = initialSuiteConfig.withFallback(runConfig).withFallback(ConfigFactory parseString rawBaseConfig).resolve()

    val wavesNodeContainer =
      new WavesNodeContainer(
        id = id,
        number = number,
        name = name,
        basePath = basePath,
        restApiPort = baseConfig.getInt("waves.rest-api.port"),
        networkApiPort = baseConfig.getInt("waves.network.port"),
        grpcApiPort = baseConfig.getInt("waves.dex.grpc.integration.port")
      )

    Seq(
      (wavesBaseConfFileName, rawBaseConfig, false),
      (s"$name.conf", getRawConfig(s"nodes/$name.conf"), false),
      ("run.conf", runConfig.resolve().root().render(), true),
      ("suite.conf", initialSuiteConfig.resolve().root().render(), true)
    ).foreach { case (fileName, content, logContent) => client.writeFile(wavesNodeContainer, Paths.get(basePath, fileName), content, logContent) }

    client.addKnownContainer(wavesNodeContainer)
    wavesNodeContainer
  }

  private def getNumber(name: String): Int = {
    val raw = name
      .split('-')
      .lastOption
      .flatMap(x => Try(x.toInt).toOption)
      .getOrElse(throw new IllegalArgumentException(s"Can't parse the container's number: '$name'. It should have a form: <name>-<number>"))

    if (raw >= 5) throw new IllegalArgumentException("All slots are filled")
    else if (name.startsWith("dex-")) raw
    else if (name.startsWith("waves-")) raw + 5
    else throw new IllegalArgumentException(s"Can't parse number from '$name'. Know 'dex-' and 'waves-' only")
  }
}
