import sbtdocker.DockerPlugin.autoImport._

enablePlugins(ItTestPlugin, JvmPlugin, sbtdocker.DockerPlugin)

description := "Integration tests of the Waves blockchain client"

libraryDependencies ++= Dependencies.Module.wavesIntegrationIt

docker := docker.dependsOn(LocalProject("waves-ext") / docker).value

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/waves-integration-it:latest")),
    dockerfile :=
      new Dockerfile {
        from("com.wavesplatform/matchernode:latest")
        user("root:root")
        add(
          sources = Seq(
            (Test / resourceDirectory).value / "nodes" / "logback-container.xml",
            (Test / sourceDirectory).value / "container" / "start-matchernode-it.sh"
          ),
          destination = "/opt/waves/"
        )
        entryPoint("/opt/waves/start-matchernode-it.sh")
      },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
