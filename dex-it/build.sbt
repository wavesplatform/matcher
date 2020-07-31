enablePlugins(ItTestPlugin, sbtdocker.DockerPlugin, ItCachedArtifactsPlugin, ImageVersionPlugin, CleanupDanglingImagesPlugin)

import CleanupDanglingImagesPlugin.autoImport.cleanupDandlingImages
import ImageVersionPlugin.autoImport.nameOfImage

description := "DEX integration tests"

libraryDependencies ++= Dependencies.Module.dexIt





val allureScalaTestVersion = "2.13.3"

val allureScalaTest = "io.qameta.allure" % "allure-scalatest_2.13" % allureScalaTestVersion % Test

testOptions in Test ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
  Tests.Argument(TestFrameworks.ScalaTest, "-C", "io.qameta.allure.scalatest.AllureScalatest")
)


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
// taskDyn motivation: https://www.scala-sbt.org/1.x/docs/Howto-Dynamic-Task.html#build.sbt+v2
docker := Def.taskDyn {
  val imageId = docker.dependsOn(downloadItArtifacts, LocalProject("waves-integration-it") / docker, LocalProject("dex") / docker).value
  Def.task { cleanupDandlingImages.value; imageId }
}.value

inTask(docker)(
  Seq(
    nameOfImage := "wavesplatform/dex-it",
    dockerfile := new Dockerfile {

      val basePath     = "/opt/waves-dex"
      val entryPointSh = s"$basePath/start-matcher-server-it.sh"

      from("wavesplatform/matcher-server:latest")
      user("root:root")

      add(
        sources = Seq(
          (Test / sourceDirectory).value / "container" / "start-matcher-server-it.sh", // entry point
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
