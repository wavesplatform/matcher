description := "Node integration extension for the Waves DEX"

import VersionSourcePlugin.V
import WavesNodeArtifactsPlugin.autoImport.wavesNodeVersion
import com.typesafe.sbt.SbtNativePackager.Universal
import sbtdocker.DockerPlugin.autoImport._

enablePlugins(RunApplicationSettings,
              WavesNodeArtifactsPlugin,
              ExtensionPackaging,
              GitVersioning,
              VersionSourcePlugin,
              JvmPlugin,
              sbtdocker.DockerPlugin)

V.scalaPackage := "com.wavesplatform.dex.grpc.integration"
V.subProject := "ext"

resolvers += "dnvriend" at "https://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.Module.wavesExt

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "Node integration extension for the Waves DEX",
  packageDescription := s"${packageSummary.value}. Compatible with ${wavesNodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

inConfig(Compile)(
  Seq(
    unmanagedJars := (Compile / unmanagedJars).dependsOn(downloadWavesNodeArtifacts).value
  )
)

// Packaging
executableScriptName := "waves-dex-extension"

// Add waves-grpc's JAR, dependency modules are ignored by ExtensionPackaging plugin
classpathOrdering += ExtensionPackaging.linkedProjectJar(
  jar = (LocalProject("waves-grpc") / Compile / packageBin).value,
  art = (LocalProject("waves-grpc") / Compile / packageBin / artifact).value,
  moduleId = (LocalProject("waves-grpc") / projectID).value
)

// Exclude waves-all*.jar
Runtime / dependencyClasspath := {
  val exclude = (Compile / unmanagedJars).value.toSet
  (Runtime / dependencyClasspath).value.filterNot(exclude.contains)
}

// ZIP archive
inConfig(Universal)(
  Seq(
    packageName := s"waves-dex-extension-${version.value}", // An archive file name
    mappings ++=
      sbt.IO
        .listFiles((Compile / packageSource).value / "doc")
        .map(file => file -> s"doc/${file.getName}")
        .toSeq,
    topLevelDirectory := None
  )
)

// DEB package
inConfig(Linux)(
  Seq(
    name := s"waves-dex-extension${network.value.packageSuffix}", // A staging directory name
    normalizedName := name.value, // An archive file name
    packageName := name.value // In a control file
  )
)

Debian / debianPackageConflicts := Seq(
  "grpc-server",
  "waves-node-grpc-server" // TODO NODE-1999
)

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/matchernode:latest")),
    dockerfile :=
      new Dockerfile {
        from(s"wavesplatform/wavesnode:${wavesNodeVersion.value}")
        user("waves:waves")
        add(
          sources = Seq((Universal / stage).value),
          destination = "/opt/waves/",
          chown = "waves:waves"
        )
        expose(6887)
      },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
