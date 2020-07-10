enablePlugins(ItTestPlugin, sbtdocker.DockerPlugin, ItCachedArtifactsPlugin, ImageVersionPlugin)

import ImageVersionPlugin.autoImport.nameOfImage

import scala.sys.process._

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

// this image hasn't a config file
docker := {
  val image = docker.dependsOn(downloadItArtifacts, LocalProject("waves-integration-it") / docker, LocalProject("dex") / docker).value
  streams.value.log.info(s"Cleaning dangling images...")
  "docker image prune -f".!
  image
}

inTask(docker)(
  Seq(
    nameOfImage := "com.wavesplatform/dex-it",
    dockerfile := new Dockerfile {

      val basePath     = "/opt/waves-dex"
      val entryPointSh = s"$basePath/start-matcherserver-it.sh"

      from("com.wavesplatform/matcherserver:latest")
      user("root:root")

      add(
        sources = Seq(
          (Test / sourceDirectory).value / "container" / "start-matcherserver-it.sh", // entry point
          (Test / resourceDirectory).value / "dex-servers" / "logback-container.xml", // logs management
          itArtifactsCacheDir.value / "aspectjweaver.jar" // profiler, see https://www.yourkit.com/docs/java/help/docker.jsp
        ),
        destination = s"$basePath/",
        chown = "waves-dex:waves-dex"
      )
      add(itArtifactsCacheDir.value / "yourKit", "/usr/local") // profiler archive

      runShell("chmod", "+x", entryPointSh)
      entryPoint(entryPointSh)
      expose(10001) // Profiler
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
