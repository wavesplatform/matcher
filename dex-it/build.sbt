import WavesExtensionDockerPlugin.autoImport._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest

val silencerVersion = "1.4.1"
libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided
)

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
