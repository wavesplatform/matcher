description := "Proto entities for DEX-KAFKA interaction"

libraryDependencies ++= Dependencies.Module.dexGrpc

scalacOptions += "-P:silencer:pathFilters=FileOptions;Endpoint;MetricDescriptor"

// Use protocGenerate to generate it manually
inConfig(Compile)(
  Seq(
    PB.deleteTargetDirectory := false,
    PB.protoSources += PB.externalIncludePath.value,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value / "scalapb"
  )
)
