import WavesExtensionDockerPlugin.autoImport._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest ++ Dependencies.silencer

docker := docker.dependsOn(buildNodeContainer).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it")),
    exposedPorts += 6886,
    additionalFiles ++= Seq(
      (LocalProject("dex") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))
