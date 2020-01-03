import sbt.Keys._
import sbt.{Def, compilerPlugin, _}

object Dependencies {

  object Version {

    val akka     = "2.5.20"
    val akkaHttp = "10.1.8"

    val scalaTest  = "3.0.6"
    val scalaCheck = "1.14.0"
    val scalaMock  = "3.6.0"
    val diffx      = "0.3.12"

    val cats              = "1.6.0"
    val catsTaglessMacros = "0.9"
    val kindProjector     = "0.9.6"
    val betterMonadicFor  = "0.3.1"
    val mouse             = "0.22"
    val shapeless         = "2.3.3"

    val typesafeConfig = "1.3.3"
    val scopt          = "4.0.0-RC2"
    val ficus          = "1.4.2"

    val logback = "1.2.3"
    val slf4j   = "1.7.25"
    val janino  = "3.1.0"

    val silencer = "1.4.1"
    val kamon    = "1.1.5"

    val wavesProtobufSchemas = "1.0.0"

    val postgresql = "9.4.1208"
    val quillJdbc  = "3.1.0"

    val sttp = "1.7.2"

    val testContainers         = "0.34.1"
    val testContainersPostgres = "1.12.3"

    val toxiProxy = "1.12.3"

    val jackson  = "2.9.8"
    val playJson = "2.7.1"

    val googleGuava = "27.0.1-jre"
    val kafka       = "2.3.1"

    val swagger = "1.1.0"

    val scorexCrypto = "2.0.4"

    val monix = "3.0.0"
  }

  private def akkaModule(module: String, version: String): ModuleID  = "com.typesafe.akka"             %% module            % version
  private def scalaModule(module: String, version: String): ModuleID = "org.scala-lang"                % module             % version
  private def catsModule(module: String, version: String): ModuleID  = "org.typelevel"                 %% s"cats-$module"   % version
  private def sttpModule(module: String): ModuleID                   = "com.softwaremill.sttp"         %% module            % Version.sttp
  private def jacksonModule(group: String, module: String): ModuleID = s"com.fasterxml.jackson.$group" % s"jackson-$module" % Version.jackson
  private def monixModule(module: String): ModuleID                  = "io.monix"                      %% s"monix-$module"  % Version.monix

  private val akkaActor              = akkaModule("akka-actor", Version.akka)
  private val akkaHttp               = akkaModule("akka-http", Version.akkaHttp)
  private val scalaTest              = "org.scalatest" %% "scalatest" % Version.scalaTest
  private val scalaCheck             = "org.scalacheck" %% "scalacheck" % Version.scalaCheck
  private val scalaMock              = "org.scalamock" %% "scalamock-scalatest-support" % Version.scalaMock
  private val diffx                  = "com.softwaremill.diffx" %% "diffx-scalatest" % Version.diffx
  private val catsCore               = catsModule("core", Version.cats)
  private val catsTaglessMacros      = "org.typelevel" %% "cats-tagless-macros" % Version.catsTaglessMacros
  private val kindProjector          = compilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)
  private val betterMonadicFor       = compilerPlugin("com.olegpy" %% "better-monadic-for" % Version.betterMonadicFor)
  private val mouse                  = "org.typelevel" %% "mouse" % Version.mouse
  private val shapeless              = "com.chuusai" %% "shapeless" % Version.shapeless
  private val typesafeConfig         = "com.typesafe" % "config" % Version.typesafeConfig
  private val scopt                  = "com.github.scopt" %% "scopt" % Version.scopt
  private val ficus                  = "com.iheart" %% "ficus" % Version.ficus
  private val logback                = "ch.qos.logback" % "logback-classic" % Version.logback
  private val slf4j                  = "org.slf4j" % "slf4j-api" % Version.slf4j
  private val logbackScalaJsExcluded = logback.exclude("org.scala-js", "scalajs-library_2.12")
  private val janino                 = "org.codehaus.janino" % "janino" % Version.janino
  private val kamon                  = "io.kamon" %% "kamon-core" % Version.kamon
  private val wavesProtobufSchemas   = ("com.wavesplatform" % "protobuf-schemas" % Version.wavesProtobufSchemas classifier "proto") % "protobuf" // for teamcity
  private val toxiProxy              = "org.testcontainers" % "toxiproxy" % Version.toxiProxy
  private val googleGuava            = "com.google.guava" % "guava" % Version.googleGuava
  private val kafka                  = "org.apache.kafka" % "kafka-clients" % Version.kafka
  private val grpcNetty              = "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion
  private val swagger                = "com.github.swagger-akka-http" %% "swagger-akka-http" % Version.swagger
  private val playJson               = "com.typesafe.play" %% "play-json" % Version.playJson
  private val scorexCrypto           = "org.scorexfoundation" %% "scrypto" % Version.scorexCrypto
  private val grpcScalaPb            = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion

  private val silencer: Seq[ModuleID] = Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % Version.silencer),
    "com.github.ghik" %% "silencer-lib" % Version.silencer % Provided
  )

  private val quill: Seq[ModuleID] = Seq(
    "org.postgresql" % "postgresql"  % Version.postgresql,
    "io.getquill"    %% "quill-jdbc" % Version.quillJdbc
  )

  private val testContainers: Seq[ModuleID] = Seq(
    "com.dimafeng"       %% "testcontainers-scala" % Version.testContainers,
    "org.testcontainers" % "postgresql"            % Version.testContainersPostgres
  )

  private val testKit: Seq[ModuleID] = Seq(
    akkaModule("akka-testkit", Version.akka),
    scalaTest,
    scalaCheck,
    scalaMock
  ) map (_ % Test)

  private val integrationTestKit: Seq[ModuleID] = Seq(logbackScalaJsExcluded % Test) ++ testKit ++ silencer

  val enforcedVersions = Def.setting(
    Seq(
      akkaActor,
      akkaHttp,
      akkaModule("akka-stream", Version.akka),
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      jacksonModule("module", "module-scala") withCrossVersion CrossVersion.Binary(),
      scalaModule("scala-library", scalaVersion.value),
      scalaModule("scala-reflect", scalaVersion.value),
      catsModule("kernel", Version.cats),
      catsModule("macros", Version.cats),
      catsCore,
      shapeless,
      kamon,
      typesafeConfig,
      scalaTest % Test,
      googleGuava,
      slf4j,
      grpcNetty
    )
  )

  object Module {

    lazy val dex: Seq[ModuleID] = Seq(
      akkaActor,
      akkaHttp,
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
    ) ++ testKit ++ quill ++ silencer

    lazy val dexCommon: Seq[ModuleID] = Seq(
      swagger,
      playJson,
      ficus,
      scorexCrypto,
      catsCore,
      monixModule("eval"),
      monixModule("reactive")
    )

    lazy val dexIt: Seq[ModuleID] = integrationTestKit

    lazy val dexItCommon: Seq[ModuleID] = Seq(
      sttpModule("core"),
      sttpModule("play-json"),
      sttpModule("async-http-client-backend-future"),
      catsCore,
      catsTaglessMacros,
      typesafeConfig,
      mouse,
      scalaTest,
      toxiProxy
    ) ++ testContainers

    lazy val dexTestCommon: Seq[ModuleID] = Seq(diffx)

    lazy val wavesExt: Seq[ModuleID] = Seq(grpcNetty)

    lazy val wavesGrpc: Seq[ModuleID] = Seq(wavesProtobufSchemas, grpcScalaPb)

    lazy val wavesIntegration: Seq[ModuleID] = Seq(
      betterMonadicFor,
      mouse % Test
    )

    lazy val wavesIntegrationIt: Seq[ModuleID] = integrationTestKit
  }
}
