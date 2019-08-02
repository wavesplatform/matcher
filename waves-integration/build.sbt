name := "waves-integration"

enablePlugins(RunApplicationSettings, ExtensionPackaging, GitVersioning)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++=
  Seq(
    Dependencies.grpc,
    Seq(Dependencies.mouse)
  ).flatten

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "waves-integration",
  packageDescription := s"Node integration extension for the Waves DEX. Compatible with ${nodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

lazy val versionSourceTask = Def.task {

  val versionFile      = sourceManaged.value / "com" / "wavesplatform" / "dex" / "grpc" / "integration" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r

  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case x                            => throw new IllegalStateException(s"Can't parse version: $x")
  }

  IO.write(
    versionFile,
    s"""package com.wavesplatform.dex.grpc.integration
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
  )
  Seq(versionFile)
}

inConfig(Compile)(
  Seq(
    sourceGenerators += versionSourceTask,
    PB.deleteTargetDirectory := false,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)
