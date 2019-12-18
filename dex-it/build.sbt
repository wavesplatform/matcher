import WavesExtensionDockerPlugin.autoImport._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest

docker := docker.dependsOn(buildNodeContainer).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it")),
    exposedPorts := Set(6886),
    additionalFiles ++= Seq(
      (LocalProject("dex") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / resourceDirectory).value / "logback-container.xml",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))

javaOptions in Test += s"-Dlogback.configurationFile=${(Test / resourceDirectory).value / "logback-test.xml"}"
