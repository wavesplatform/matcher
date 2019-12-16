package com.wavesplatform.it.docker

import java.io.FileNotFoundException
import java.nio.file.Paths

import com.typesafe.config.Config
import com.wavesplatform.dex.it.docker.Docker

import scala.io.Source
import scala.util.Try

object DexItDocker {

  private val restApiPort         = 6886 // application.conf waves.dex.rest-api.port
  private val dexBaseConfFileName = "dex-base.conf"
  private val dexImage            = "com.wavesplatform/dex-it:latest"

  private def getRawContentFromResource(fileName: String): String = {
    Try(Source fromResource fileName).getOrElse { throw new FileNotFoundException(s"Resource '$fileName'") }.mkString
  }

  def createContainer(client: Docker)(name: String, runConfig: Config, initialSuiteConfig: Config): DexContainer = {
    val number   = getNumber(name)
    val basePath = "/opt/waves-dex"

    val id =
      client.create(
        number = number,
        name = name,
        imageName = dexImage,
        env = Map(
          "WAVES_DEX_CONFIGPATH" -> s"$basePath/$name.conf",
          "WAVES_DEX_OPTS"       -> Seq(
            "-Dlogback.stdout.enable=false",
            "-Dlogback.file.enable=false",
            "-Dlogback.configurationFile=/opt/waves-dex/doc/logback.xml",
            "-Dlogback.include.file=/opt/waves-dex/doc/logback-container.xml"
          ).mkString(" ", " ", " ")
        )
      )

    val dexContainer =
      new DexContainer(
        id = id,
        number = number,
        name = name,
        basePath = basePath,
        restApiPort = restApiPort
      )

    Seq(
      (dexBaseConfFileName, getRawContentFromResource(s"dex-servers/$dexBaseConfFileName"), false),
      (s"$name.conf", getRawContentFromResource(s"dex-servers/$name.conf"), false),
      ("run.conf", runConfig.resolve().root().render(), true),
      ("suite.conf", initialSuiteConfig.resolve().root().render(), true)
    ).foreach { case (fileName, content, logContent) => client.writeFile(dexContainer, Paths.get(basePath, fileName), content, logContent) }

    client.writeFile(
      container = dexContainer,
      to = Paths.get("/opt/waves-dex/doc/logback-container.xml"),
      content = getRawContentFromResource("logback-container.xml")
    )

    client.addKnownContainer(dexContainer)
    dexContainer
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
