import java.nio.charset.StandardCharsets

import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.DebianConstants._

enablePlugins(RunApplicationSettings, ExtensionPackaging, GitVersioning)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.dex

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "DEX",
  packageDescription := s"Decentralized EXchange for Waves network. Compatible with ${nodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

lazy val versionSourceTask = Def.task {
  val versionFile      = sourceManaged.value / "com" / "wavesplatform" / "dex" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case x                            => throw new IllegalStateException(s"Can't parse version: $x")
  }

  IO.write(
    versionFile,
    s"""package com.wavesplatform.dex
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin,
    charset = StandardCharsets.UTF_8
  )
  Seq(versionFile)
}

inConfig(Compile)(
  Seq(
    sourceGenerators += versionSourceTask,
    PB.deleteTargetDirectory := false,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  ))

Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postrm)(
  Postrm ->
    s"""#!/bin/sh
       |set -e
       |if [ "$$1" = purge ]; then
       |  rm -rf /var/lib/${nodePackageName.value}/matcher
       |fi""".stripMargin
)
