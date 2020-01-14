import sbt.Keys._
import sbt.{Def, _}

object Dependencies {

  object Version {

    val akka     = "2.5.20"
    val akkaHttp = "10.1.8"

    val scalaTest  = "3.0.6"
    val scalaCheck = "1.14.0"
    val scalaMock  = "3.6.0"
    val diffx      = "0.3.12"

    val cats              = "2.0.0"
    val catsTaglessMacros = "0.9"
    val kindProjector     = "0.9.6"
    val mouse             = "0.22"
    val shapeless         = "2.3.3"

    val typesafeConfig = "1.3.3"
    val scopt          = "4.0.0-RC2"

    val logback = "1.2.3"
    val slf4j   = "1.7.25"
    val janino  = "3.1.0"

    val silencer = "1.4.1"
    val kamon    = "1.1.6"

    val wavesProtobufSchemas = "1.0.0"

    val postgresql = "9.4.1208"
    val quillJdbc  = "3.1.0"

    val sttp = "1.7.2"

    val testContainers         = "0.34.1"
    val testContainersPostgres = "1.12.3"

    val toxiProxy = "1.12.3"

    val jackson     = "2.9.8"
    val googleGuava = "27.0.1-jre"
    val kafka       = "2.3.1"

    val sourceCode = "0.1.7"
  }

  private def akkaModule(module: String, version: String): ModuleID  = "com.typesafe.akka"             %% module            % version
  private def scalaModule(module: String, version: String): ModuleID = "org.scala-lang"                % module             % version
  private def jacksonModule(group: String, module: String): ModuleID = s"com.fasterxml.jackson.$group" % s"jackson-$module" % Version.jackson
  private def sttpModule(module: String): ModuleID                   = "com.softwaremill.sttp"         %% module            % Version.sttp

  private def catsModule(module: String, version: String): Def.Initialize[ModuleID] = Def.setting("org.typelevel" %% s"cats-$module" % version)

  private val kindProjector     = compilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)
  private val logback           = "ch.qos.logback" % "logback-classic" % Version.logback
  private val googleGuava       = "com.google.guava" % "guava" % Version.googleGuava
  private val janino            = "org.codehaus.janino" % "janino" % Version.janino
  private val typesafeConfig    = "com.typesafe" % "config" % Version.typesafeConfig
  private val scalaTest         = "org.scalatest" %% "scalatest" % Version.scalaTest
  private val scalaCheck        = "org.scalacheck" %% "scalacheck" % Version.scalaCheck
  private val scalaMock         = "org.scalamock" %% "scalamock-scalatest-support" % Version.scalaMock
  private val diffx             = "com.softwaremill.diffx" %% "diffx-scalatest" % Version.diffx
  private val slf4j             = "org.slf4j" % "slf4j-api" % Version.slf4j
  private val grpcNetty         = "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion
  private val catsTaglessMacros = "org.typelevel" %% "cats-tagless-macros" % Version.catsTaglessMacros
  private val mouse             = "org.typelevel" %% "mouse" % Version.mouse
  private val scopt             = "com.github.scopt" %% "scopt" % Version.scopt
  private val kafka             = "org.apache.kafka" % "kafka-clients" % Version.kafka
  private val kamon             = "io.kamon" %% "kamon-core" % Version.kamon
  private val toxiProxy         = "org.testcontainers" % "toxiproxy" % Version.toxiProxy

  private val logbackScalaJsExcluded = logback.exclude("org.scala-js", "scalajs-library_2.12")

  private val catsCore: Def.Initialize[ModuleID]  = catsModule("core", Version.cats)
  private val shapeless: Def.Initialize[ModuleID] = Def.setting("com.chuusai" %% "shapeless" % Version.shapeless)

  private val quill: Seq[ModuleID] = Seq(
    "org.postgresql" % "postgresql"  % Version.postgresql,
    "io.getquill"    %% "quill-jdbc" % Version.quillJdbc
  )

  private val testContainers: Seq[ModuleID] = Seq(
    "com.dimafeng"       %% "testcontainers-scala" % Version.testContainers,
    "org.testcontainers" % "postgresql"            % Version.testContainersPostgres
  )

  val silencer: Seq[ModuleID] = Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % Version.silencer),
    "com.github.ghik" %% "silencer-lib" % Version.silencer % Provided
  )

  val enforcedVersions = Def.setting(
    Seq(
      akkaModule("akka-actor", Version.akka),
      akkaModule("akka-stream", Version.akka),
      akkaModule("akka-http", Version.akkaHttp),
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      jacksonModule("module", "module-scala") withCrossVersion CrossVersion.Binary(),
      scalaModule("scala-library", scalaVersion.value),
      scalaModule("scala-reflect", scalaVersion.value),
      catsModule("kernel", Version.cats).value,
      catsModule("macros", Version.cats).value,
      catsCore.value,
      shapeless.value,
      kamon,
      typesafeConfig,
      scalaTest % Test,
      googleGuava,
      slf4j,
      grpcNetty
    )
  )

  lazy val common: Seq[ModuleID] = Seq("com.lihaoyi" %% "sourcecode" % Version.sourceCode)

  lazy val wavesProtobufSchemas: ModuleID = {
    ("com.wavesplatform" % "protobuf-schemas" % Version.wavesProtobufSchemas classifier "proto") % "protobuf" // for teamcity
  }

  lazy val grpc: Seq[ModuleID] = Seq(
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    grpcNetty
  )

  lazy val itTestCommon: Def.Initialize[Seq[ModuleID]] = Def.setting(
    Seq(
      scalaTest,
      sttpModule("core"),
      sttpModule("play-json"),
      sttpModule("async-http-client-backend-future"),
      catsCore.value,
      catsTaglessMacros,
      typesafeConfig,
      mouse,
      toxiProxy
    ) ++ testContainers
  )

  lazy val testCommon: Def.Initialize[Seq[ModuleID]] = Def.setting(Seq(diffx))

  lazy val itTest: Seq[ModuleID] = Seq(
    jacksonModule("dataformat", "dataformat-properties"),
    logbackScalaJsExcluded,
    scalaTest,
    scalaCheck
  ) map (_ % Test)

  lazy val test: Seq[ModuleID] = Seq(
    akkaModule("akka-testkit", Version.akka),
    scalaTest,
    scalaCheck,
    scalaMock
  ) map (_ % Test)

  lazy val dex: Seq[ModuleID] = Seq(
    akkaModule("akka-actor", Version.akka),
    akkaModule("akka-http", Version.akkaHttp),
    akkaModule("akka-slf4j", Version.akka),
    wavesProtobufSchemas,
    logbackScalaJsExcluded,
    logback,
    kindProjector,
    catsTaglessMacros,
    mouse,
    scopt,
    kafka,
    janino
  ) ++ test ++ quill

  lazy val wavesIntegration: Seq[ModuleID] = Seq(
    wavesProtobufSchemas,
    mouse % Test
  ) ++ grpc
}
