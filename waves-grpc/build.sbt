description := "Proto files and generated gRPC entities and servers for DEX-Node interaction"

libraryDependencies ++= Dependencies.Module.wavesGrpc

// Google's descriptor.proto contains a deprecated field:
//  optional bool java_generate_equals_and_hash = 20 [deprecated=true];
// When scalac compiles a generated class, it warns about a deprecated field.
scalacOptions += "-P:silencer:pathFilters=FileOptions;TransactionsApiGrpc;InvokeScriptResult"

// Use protocGenerate to generate it manually
inConfig(Compile)(
  Seq(
    PB.deleteTargetDirectory := false,
    PB.protoSources += PB.externalIncludePath.value,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value / "scalapb"
  )
)
