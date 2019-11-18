import java.nio.charset.StandardCharsets

import DexDockerKeys._
import com.typesafe.sbt.SbtNativePackager.Universal

enablePlugins(JavaServerAppPackaging, UniversalDeployPlugin, JDebPackaging, SystemdPlugin, DexDockerPlugin, RunApplicationSettings, GitVersioning)

resolvers += "dnvriend" at "https://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.dex ++ Dependencies.silencer

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  name := "waves-dex",
  packageSummary := "DEX",
  packageDescription := "Decentralized EXchange for Waves network",
  // For sbt-native-packager
  executableScriptName := packageName.value,
  normalizedName := name.value
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

inConfig(Universal)(
  packageSettings ++ Seq(
    mappings ++= {
      val baseConfigName = s"${network.value}.conf"
      val localFile      = (Compile / sourceDirectory).value / "package" / "samples" / baseConfigName
      if (localFile.exists()) {
        val artifactPath = "doc/dex.conf.sample"
        Seq(localFile -> artifactPath)
      } else Seq.empty
    }
  ))

inConfig(Linux)(packageSettings)
inConfig(Debian)(inTask(packageBin)(packageSettings) ++ packageSettings)
