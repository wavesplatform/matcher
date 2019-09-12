package com.wavesplatform.it.docker

import java.io.FileNotFoundException
import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.docker.Docker

import scala.io.Source
import scala.util.Try

object DexItDocker {
  private val dexImage = "com.wavesplatform/dex-it:latest"

  def createContainer(client: Docker)(name: String, runConfig: Config, initialSuiteConfig: Config): DexContainer = {
    val number   = getNumber(name)
    val basePath = "/opt/waves-dex"
    val id = client.create(
      number,
      name,
      dexImage,
      Map(
        "WAVES_DEX_CONFIGPATH" -> s"$basePath/$name.conf",
        "WAVES_DEX_OPTS"       -> s"-Dlogback.configurationFile=$basePath/logback.xml"
      )
    )

    val rawBaseConfig = Try(Source.fromResource(s"nodes/$name.conf"))
      .getOrElse(throw new FileNotFoundException(s"Resource 'nodes/$name.conf'"))
      .mkString

    val baseConfig = initialSuiteConfig.withFallback(runConfig).withFallback(ConfigFactory.parseString(rawBaseConfig)).resolve()
    val r = new DexContainer(
      id = id,
      number = number,
      name = name,
      basePath = basePath,
      restApiPort = baseConfig.getInt("waves.dex.rest-api.port")
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
