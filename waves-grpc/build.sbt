description := "Proto files and generated gRPC entities and servers for DEX-Node interaction"

libraryDependencies ++= Dependencies.Module.wavesGrpc

inConfig(Compile)(
  Seq(
    PB.deleteTargetDirectory := false,
    PB.protoSources in Compile += PB.externalIncludePath.value,
    PB.targets += scalapb.gen(flatPackage = true) -> (Compile / sourceManaged).value
  )
)
