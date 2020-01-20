import DexItDockerKeys._

enablePlugins(DexItDockerPlugin, ItTestPlugin)

description := "DEX integration tests"

libraryDependencies ++= Dependencies.Module.dexIt

// this image hasn't a config file
docker := docker.dependsOn(LocalProject("waves-integration-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it:latest")),
    exposedPorts += 6886,
    additionalFiles ++= Seq(
      (Test / resourceDirectory).value / "dex-servers" / "logback-container.xml"
    )
  )
)
