import java.nio.charset.StandardCharsets

import Dependencies.Version
import VersionSourcePlugin.V
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.plugins.JvmPlugin

enablePlugins(
  RewriteSwaggerConfigPlugin,
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  JDebPackaging,
  SystemdPlugin,
  GitVersioning,
  VersionSourcePlugin,
  JvmPlugin,
  sbtdocker.DockerPlugin
)

V.scalaPackage := "com.wavesplatform.dex"
V.subProject := "dex"

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
    imageNames := {
      val latestImageName = ImageName("com.wavesplatform/matcherserver:latest")
      val maybeVersion    = git.gitDescribedVersion.value
      if (!isSnapshot.value && maybeVersion.isDefined) Seq(latestImageName, ImageName(s"com.wavesplatform/matcherserver:${maybeVersion.get}"))
      else Seq(latestImageName)
    },
    dockerfile :=
      new Dockerfile {

        val (user, userId)   = ("waves-dex", "113")
        val (group, groupId) = ("waves-dex", "116")

        val userPath    = s"/var/lib/$user"
        val sourcesPath = s"/usr/share/$user"

        val entryPointSh = s"$sourcesPath/bin/start-matcherserver.sh"

        from("openjdk:8-jre-slim-buster")

        runRaw(s"""mkdir -p $userPath $sourcesPath $userPath/runtime && \\
                  |groupadd -g $groupId $group && \\
                  |useradd -d $userPath -g $groupId -u $userId -s /bin/bash -M $user && \\
                  |chown -R $userId:$groupId $userPath $sourcesPath && \\
                  |chmod -R 755 $userPath $sourcesPath && \\
                  |ln -fs $sourcesPath/log/ var/log/waves-dex""".stripMargin)

        user(s"$user:$group")

        Seq(
          (Universal / stage).value                                                  -> sourcesPath, // sources
          (Compile / sourceDirectory).value / "container" / "start-matcherserver.sh" -> s"$sourcesPath/bin/", // entry point
          (Compile / sourceDirectory).value / "container" / "dex.conf"               -> s"$sourcesPath/conf/" // base config
        ) foreach { case (source, destination) => add(source = source, destination = destination, chown = s"$user:$group") }

        runShell("chmod", "+x", entryPointSh)
        workDir(userPath)
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
    javaOptions ++= Seq("-Xmx2g", "-Xms128m").map(x => s"-J$x"),
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
    packageName := name.value // In a control file
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
        val src    = packageSource.value / "upstart.conf"
        val dest   = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
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
