import java.nio.charset.StandardCharsets

import DexDockerKeys._
import com.typesafe.sbt.SbtNativePackager.Universal
import CommonSettings.autoImport.network

enablePlugins(JavaServerAppPackaging, UniversalDeployPlugin, JDebPackaging, SystemdPlugin, DexDockerPlugin, RunApplicationSettings, GitVersioning)

resolvers += "dnvriend" at "https://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.dex ++ Dependencies.silencer

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "DEX",
  packageDescription := "Decentralized EXchange for Waves network"
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
    discoveredMainClasses := Seq(
      "com.wavesplatform.dex.Application",
      "com.wavesplatform.dex.WavesDexCli"
    ),
    mainClass := discoveredMainClasses.value.headOption
  ))

// Docker

inTask(docker)(
  Seq(
    additionalFiles ++= Seq(
      (Universal / stage).value,
      (Compile / sourceDirectory).value / "container" / "start.sh",
      (Compile / sourceDirectory).value / "container" / "default.conf"
    )
  )
)

// Packaging

executableScriptName := "waves-dex"

// ZIP archive
inConfig(Universal)(Seq(
  packageName := s"waves-dex${network.value.packageSuffix}-${version.value}", // An archive file name
  mappings ++= {
    val baseConfigName = s"${network.value}.conf"
    val localFile      = (Compile / sourceDirectory).value / "package" / "samples" / baseConfigName
    if (localFile.exists()) {
      val artifactPath = "doc/dex.conf.sample"
      Seq(localFile -> artifactPath)
    } else Seq.empty
  }
))

// DEB package
Linux / name := s"waves-dex${network.value.packageSuffix}" // A staging directory name
Linux / normalizedName := (Linux / name).value // An archive file name
Linux / packageName := (Linux / name).value // In a control file
