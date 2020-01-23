import WavesExtensionDockerKeys._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "Integration tests of the Waves blockchain client"

libraryDependencies ++= Dependencies.Module.wavesIntegrationIt

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/waves-integration-it:latest")),
    exposedPorts := Set(6887),
    additionalFiles ++= Seq(
      (LocalProject("waves-ext") / Universal / stage).value,
      (Test / resourceDirectory).value / "nodes" / "logback-container.xml",
      (Test / sourceDirectory).value / "container" / "start-waves.sh"
    )
  )
)
