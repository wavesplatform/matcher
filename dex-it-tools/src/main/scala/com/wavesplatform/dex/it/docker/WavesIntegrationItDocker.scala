package com.wavesplatform.dex.it.docker

import java.io.FileNotFoundException
import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}

import scala.io.Source
import scala.util.Try

object WavesIntegrationItDocker {
  private val wavesNodeImage = "com.wavesplatform/waves-integration-it:latest"

  def createContainer(client: Docker)(name: String, runConfig: Config, initialSuiteConfig: Config): WavesNodeContainer = {
    val number   = getNumber(name)
    val basePath = "/opt/waves"
    val id = client.create(
      number,
      name,
      wavesNodeImage,
      Map(
        "WAVES_NODE_CONFIGPATH" -> s"$basePath/$name.conf",
        "WAVES_OPTS"            -> s"-Dlogback.configurationFile=$basePath/logback.xml"
      )
    )

    val rawBaseConfig = Try(Source.fromResource(s"nodes/$name.conf"))
      .getOrElse(throw new FileNotFoundException(s"Resource 'nodes/$name.conf'"))
      .mkString

    val baseConfig = initialSuiteConfig.withFallback(runConfig).withFallback(ConfigFactory.parseString(rawBaseConfig)).resolve()
    val r = new WavesNodeContainer(
      id = id,
      number = number,
      name = name,
      basePath = basePath,
      restApiPort = baseConfig.getInt("waves.rest-api.port"),
      networkApiPort = baseConfig.getInt("waves.network.port"),
      grpcApiPort = baseConfig.getInt("waves.dex.grpc.integration.port")
    )
    Map(
      s"$name.conf" -> rawBaseConfig,
      "run.conf"    -> runConfig.resolve().root().render(),
      "suite.conf"  -> initialSuiteConfig.resolve().root().render()
    ).foreach { case (fileName, content) => client.writeFile(r, Paths.get(basePath, fileName), content) }

    client.addKnownContainer(r)
    r
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
