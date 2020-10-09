import sbtdocker.DockerPlugin.autoImport._

enablePlugins(ItTestPlugin, sbtdocker.DockerPlugin, ImageVersionPlugin)

import ImageVersionPlugin.autoImport.{imageTagMakeFunction, nameOfImage}

description := "Integration tests of the Waves blockchain client"

libraryDependencies ++= Dependencies.Module.wavesIntegrationIt

docker := docker.dependsOn(LocalProject("waves-ext") / docker).value

inTask(docker)(
  Seq(
    nameOfImage := "wavesplatform/waves-integration-it",
    imageTagMakeFunction := (gitTag => s"${wavesNodeVersion.value}_$gitTag"),
    dockerfile := new Dockerfile {

      val basePath = "/opt/waves"
      val entryPointSh = s"$basePath/start-matcher-node-it.sh"

      from("wavesplatform/matcher-node:latest")
      user("root:root")

      add(
        sources = Seq(
          (Test / sourceDirectory).value / "container" / "start-matcher-node-it.sh", // entry point
          (Test / resourceDirectory).value / "nodes" / "logback-container.xml" // logs management
        ),
        destination = s"$basePath/",
        chown = "143:143"
      )

      runShell("chmod", "+x", entryPointSh)
      entryPoint(entryPointSh)
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
