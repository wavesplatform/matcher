description := "DEX domain entities, cryptography and blockchain client for DEX-Node interaction"

libraryDependencies ++= Dependencies.Module.wavesIntegration

inConfig(Compile)(
  Seq(
    PB.deleteTargetDirectory := false,
    PB.protoSources += PB.externalIncludePath.value,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value / "scalapb"
  )
)
