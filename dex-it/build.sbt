enablePlugins(ItTestPlugin, sbtdocker.DockerPlugin, ItCachedArtifactsPlugin, ImageVersionPlugin)

import ImageVersionPlugin.autoImport.nameOfImage

description := "DEX integration tests"

libraryDependencies ++= Dependencies.Module.dexIt

// Additional files required for dex-it image
itArtifactDescriptions := {

  val yourKitArchive = "YourKit-JavaProfiler-2019.8-docker.zip"
  val cachedDir      = itArtifactsCacheDir.value

  Seq(
    ArtifactDescription(
      downloadUrl = "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar",
      name = Some("aspectjweaver.jar")
    ),
    ArtifactDescription(
      downloadUrl = s"https://www.yourkit.com/download/docker/$yourKitArchive",
      additionalScript = Some(s"unzip $cachedDir/$yourKitArchive -d $cachedDir/yourKit")
    )
  )
}

docker := docker.dependsOn(downloadItArtifacts, LocalProject("waves-integration-it") / docker, LocalProject("dex") / docker).value
inTask(docker)(
  Seq(
    nameOfImage := "wavesplatform/dex-it",
    dockerfile := new Dockerfile {
      val appPath      = "/usr/share/waves-dex"
      val entryPointSh = s"$appPath/bin/start-matcher-server-it.sh"

      from("wavesplatform/matcher-server:latest")
      user("root:root")

      List(
        (Test / sourceDirectory).value / "container" / "start-matcher-server-it.sh" -> s"$appPath/bin/", // entry point
        (Test / resourceDirectory).value / "dex-servers" / "logback-container.xml"  -> s"$appPath/doc/", // logs management
        itArtifactsCacheDir.value / "aspectjweaver.jar"                             -> s"$appPath/lib", // profiler, see https://www.yourkit.com/docs/java/help/docker.jsp
        itArtifactsCacheDir.value / "yourKit"                                       -> "/usr/local" // profiler archive
      ).foreach {
        case (src, dest) => add(src, dest, chown = "waves-dex:waves-dex")
      }

      runShell("chmod", "+x", entryPointSh)
      entryPoint(entryPointSh)
      expose(10001) // Profiler
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
