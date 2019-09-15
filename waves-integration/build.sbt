name := "waves-integration"

import sbtassembly.MergeStrategy

enablePlugins(RunApplicationSettings, ExtensionPackaging, GitVersioning)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.wavesIntegration

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "waves-integration",
  packageDescription := s"Node integration extension for the Waves DEX. Compatible with ${nodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

lazy val versionSourceTask = Def.task {

  val versionFile      = sourceManaged.value / "com" / "wavesplatform" / "dex" / "grpc" / "integration" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r

  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case x                            => throw new IllegalStateException(s"Can't parse version: $x")
  }

  IO.write(
    versionFile,
    s"""package com.wavesplatform.dex.grpc.integration
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
  )
  Seq(versionFile)
}

inConfig(Compile)(
  Seq(
    sourceGenerators += versionSourceTask,
    PB.deleteTargetDirectory := false,
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)

val aopMerge: MergeStrategy = new MergeStrategy {

  import scala.xml._
  import scala.xml.dtd._

  override val name = "aopMerge"

  override def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val dt                         = DocType("aspectj", PublicID("-//AspectJ//DTD//EN", "http://www.eclipse.org/aspectj/dtd/aspectj.dtd"), Nil)
    val file                       = MergeStrategy.createMergeTarget(tempDir, path)
    val xmls: Seq[Elem]            = files.map(XML.loadFile)
    val aspectsChildren: Seq[Node] = xmls.flatMap(_ \\ "aspectj" \ "aspects" \ "_")
    val weaverChildren: Seq[Node]  = xmls.flatMap(_ \\ "aspectj" \ "weaver" \ "_")
    val options: String            = xmls.map(x => (x \\ "aspectj" \ "weaver" \ "@options").text).mkString(" ").trim
    val weaverAttr                 = if (options.isEmpty) Null else new UnprefixedAttribute("options", options, Null)
    val aspects                    = new Elem(null, "aspects", Null, TopScope, false, aspectsChildren: _*)
    val weaver                     = new Elem(null, "weaver", weaverAttr, TopScope, false, weaverChildren: _*)
    val aspectj                    = new Elem(null, "aspectj", Null, TopScope, false, aspects, weaver)
    XML.save(file.toString, aspectj, "UTF-8", xmlDecl = false, dt)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

inTask(assembly)(
  Seq(
    test := {},
    assemblyJarName := s"waves-all-${version.value}.jar",
    assemblyMergeStrategy := {
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat
      case PathList("META-INF", "aop.xml")                      => aopMerge
      case PathList("logback.xml")                              => MergeStrategy.first
      case other                                                => (assemblyMergeStrategy in assembly).value(other)
    },
    mainClass := Some("com.wavesplatform.dex.grpc.integration.Main")
  )
)
