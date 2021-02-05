import java.nio.charset.StandardCharsets
import Dependencies.Version
import ImageVersionPlugin.autoImport.nameOfImage
import VersionSourcePlugin.V
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys.javaOptions

enablePlugins(
  RewriteSwaggerConfigPlugin,
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  JDebPackaging,
  SystemdPlugin,
  GitVersioning,
  VersionSourcePlugin,
  sbtdocker.DockerPlugin,
  ImageVersionPlugin
)

V.scalaPackage := "com.wavesplatform.dex"
V.subProject := "dex"

Test / scalacOptions += "-P:silencer:globalFilters=^magnolia: using fallback derivation.*$" // https://github.com/softwaremill/diffx#customization

resolvers += "dnvriend" at "https://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.Module.dex

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "DEX",
  packageDescription := "Decentralized EXchange for Waves network"
)

packageSettings
inScope(Global)(packageSettings)

lazy val swaggerUiVersionSourceTask = Def.task {
  val versionFile = sourceManaged.value / "com" / "wavesplatform" / "dex" / "api" / "http" / "SwaggerUiVersion.scala"
  IO.write(
    versionFile,
    s"""package com.wavesplatform.dex.api.http
       |
       |object SwaggerUiVersion {
       |  val VersionString = "${Version.swaggerUi}"
       |}
       |""".stripMargin,
    charset = StandardCharsets.UTF_8
  )
  Seq(versionFile)
}

inConfig(Compile)(
  Seq(
    sourceGenerators += swaggerUiVersionSourceTask.taskValue,
    discoveredMainClasses := Seq(
      "com.wavesplatform.dex.Application",
      "com.wavesplatform.dex.cli.WavesDexCli"
    ),
    mainClass := discoveredMainClasses.value.headOption,
    run / fork := true
  )
)

// Docker
inTask(docker)(
  Seq(
    nameOfImage := "wavesplatform/matcher-server",
    dockerfile := new Dockerfile {

      val (user, userId) = ("waves-dex", "113")
      val (group, groupId) = ("waves-dex", "116")

      val runtimePath = s"/var/lib/$user"
      val appPath = s"/usr/share/$user"

      val entryPointSh = s"$appPath/bin/start-matcher-server.sh"

      from("openjdk:8-jre-slim-buster")

      runRaw(s"""mkdir -p $runtimePath $appPath $runtimePath/runtime && \\
                |groupadd -g $groupId $group && \\
                |useradd -d $runtimePath -g $groupId -u $userId -s /bin/bash -M $user && \\
                |chown -R $userId:$groupId $runtimePath $appPath && \\
                |chmod -R 755 $runtimePath $appPath && \\
                |ln -fs $runtimePath/log var/log/waves-dex""".stripMargin)

      Seq(
        (Universal / stage).value -> s"$appPath/", // sources
        (Compile / sourceDirectory).value / "container" / "start-matcher-server.sh" -> s"$appPath/bin/", // entry point
        (Compile / sourceDirectory).value / "container" / "dex.conf" -> s"$appPath/conf/" // base config
      ) foreach { case (source, destination) => add(source = source, destination = destination, chown = s"$userId:$groupId") }

      user(s"$userId:$groupId")

      runShell("chmod", "+x", entryPointSh)
      workDir(runtimePath)
      entryPoint(entryPointSh)
      expose(6886)
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)

// Packaging
executableScriptName := "waves-dex"

// ZIP archive and mappings for all artifacts
inConfig(Universal)(
  Seq(
    packageName := s"waves-dex-${version.value}", // An archive file name
    // Common JVM parameters
    // -J prefix is required by a parser
    javaOptions ++= Seq("-Xmx2g", "-Xms512m").map(x => s"-J$x"),
    mappings ++=
      sbt.IO
        .listFiles((Compile / packageSource).value / "doc")
        .map(file => file -> s"doc/${file.getName}")
        .toSeq
  )
)

// DEB package
inConfig(Linux)(
  Seq(
    name := "waves-dex", // A staging directory name
    normalizedName := name.value, // An archive file name
    packageName := name.value, // In a control file
  )
)

inConfig(Debian)(
  Seq(
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java8-runtime-headless",
    serviceAutostart := false,
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm")),
    linuxPackageMappings ++= {
      val upstartScript = {
        val src = packageSource.value / "upstart.conf"
        val dest = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
        val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
        IO.write(dest, result)
        dest
      }

      Seq(upstartScript -> s"/etc/init/${packageName.value}.conf").map(packageMapping(_).withConfig().withPerms("644"))
    },
    linuxScriptReplacements += "detect-loader" ->
    """is_systemd() {
      |    which systemctl >/dev/null 2>&1 && \
      |    systemctl | grep -- -\.mount >/dev/null 2>&1
      |}
      |is_upstart() {
      |    /sbin/init --version | grep upstart >/dev/null 2>&1
      |}
      |""".stripMargin
  )
)
