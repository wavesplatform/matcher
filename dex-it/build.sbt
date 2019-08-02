import DexItDockerKeys._

enablePlugins(DexItDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest ++ Dependencies.silencer

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/waves-dex-it")),
    exposedPorts += 6886,
    additionalFiles ++= Seq(
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))
