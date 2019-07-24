import WavesExtensionDockerPlugin.autoImport._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "Integration tests of the DEX gRPC extension for the Waves node "

libraryDependencies ++= Dependencies.itTest

docker := docker.dependsOn(buildNodeContainer).value

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/waves-integration-it")),
    exposedPorts := Set(6887),
    additionalFiles ++= Seq(
      (LocalProject("waves-integration") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  )
)
