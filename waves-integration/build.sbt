description := "DEX domain entities, cryptography and blockchain client for DEX-Node interaction"

libraryDependencies ++= Dependencies.Module.wavesIntegration

// Google's descriptor.proto contains a deprecated field:
//  optional bool java_generate_equals_and_hash = 20 [deprecated=true];
// When scalac compiles a generated class, it warns about a deprecated field.
scalacOptions += "-P:silencer:pathFilters=FileOptions"

inConfig(Compile)(
  Seq(
    PB.deleteTargetDirectory := false,
    PB.protoSources += PB.externalIncludePath.value,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value / "scalapb"
  )
)
