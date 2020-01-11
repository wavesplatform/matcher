import WavesExtensionDockerKeys._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "Integration tests of the DEX gRPC extension for the Waves node"

libraryDependencies ++= Dependencies.itTest ++ Dependencies.silencer

docker := docker.dependsOn(buildNodeContainer).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/waves-integration-it:latest")),
    exposedPorts := Set(6887),
    additionalFiles ++= Seq(
      (LocalProject("waves-integration") / Universal / stage).value,
      (Test / resourceDirectory).value / "nodes" / "logback-container.xml",
      (Test / sourceDirectory).value / "container" / "start-waves.sh"
    )
  )
)
