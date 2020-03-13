import sbt.Keys._
import sbt.{Def, compilerPlugin, _}

object Dependencies {

  object Version {

    val akka     = "2.6.1"
    val akkaHttp = "10.1.11"

    val scalaTest          = "3.1.0"
    val scalaCheck         = "1.14.3"
    val scalaTestPlusCheck = "3.1.0.1"
    val scalaMock          = "4.4.0"
    val diffx              = "0.3.16"

    val cats              = "2.0.0"
    val catsTaglessMacros = "0.11"
    val kindProjector     = "0.9.10"
    val betterMonadicFor  = "0.3.1"
    val mouse             = "0.24"
    val shapeless         = "2.3.3"

    val typesafeConfig = "1.4.0"
    val scopt          = "4.0.0-RC2"
    val ficus          = "1.4.7"

    val logback            = "1.2.3"
    val slf4j              = "1.7.30"
    val janino             = "3.1.0"
    val logbackJsonEncoder = "6.3"

    val silencer = "1.4.4"

    val kamonCore          = "1.1.6"
    val kamonInfluxDb      = "1.0.3"
    val kamonSystemMetrics = "1.0.1"

    val wavesProtobufSchemas = "1.0.0"
    val wavesJ               = "0.16.0"

    val postgresql = "42.2.9"
    val quillJdbc  = "3.5.0"

    val sttp = "1.7.2"

    val testContainers          = "0.35.2"
    val testContainersPostgres  = "1.12.5"
    val testContainersToxiProxy = "1.12.5"

    val jackson  = "2.10.0"
    val playJson = "2.8.1"

    val googleGuava = "28.2-jre"
    val kafka       = "2.4.0"

    val swagger   = "1.1.1"
    val swaggerUi = "3.24.3"

    val scorexCrypto = "2.1.7"

    val monix = "3.0.0"

    val supertagged = "1.4"

    val javaLevelDb = "0.12"
    val jniLevelDb  = "1.18.3"
    val influxDb    = "2.17"

    val commonsNet = "3.6"
  }

  private def akkaModule(module: String, version: String): ModuleID  = "com.typesafe.akka"             %% module            % version
  private def scalaModule(module: String, version: String): ModuleID = "org.scala-lang"                % module             % version
  private def catsModule(module: String): ModuleID                   = "org.typelevel"                 %% s"cats-$module"   % Version.cats
  private def sttpModule(module: String): ModuleID                   = "com.softwaremill.sttp"         %% module            % Version.sttp
  private def jacksonModule(group: String, module: String): ModuleID = s"com.fasterxml.jackson.$group" % s"jackson-$module" % Version.jackson
  private def monixModule(module: String): ModuleID                  = "io.monix"                      %% s"monix-$module"  % Version.monix
  private def kamonModule(module: String, version: String): ModuleID = "io.kamon"                      %% s"kamon-$module"  % version

  private val akkaActor            = akkaModule("akka-actor", Version.akka)
  private val akkaHttp             = akkaModule("akka-http", Version.akkaHttp)
  private val scalaTest            = "org.scalatest" %% "scalatest" % Version.scalaTest
  private val scalaCheck           = "org.scalacheck" %% "scalacheck" % Version.scalaCheck
  private val scalaTestPlusCheck   = "org.scalatestplus" %% "scalacheck-1-14" % Version.scalaTestPlusCheck
  private val scalaMock            = "org.scalamock" %% "scalamock" % Version.scalaMock
  private val diffx                = "com.softwaremill.diffx" %% "diffx-scalatest" % Version.diffx
  private val catsCore             = catsModule("core")
  private val catsTaglessMacros    = "org.typelevel" %% "cats-tagless-macros" % Version.catsTaglessMacros
  private val kindProjector        = compilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)
  private val betterMonadicFor     = compilerPlugin("com.olegpy" %% "better-monadic-for" % Version.betterMonadicFor)
  private val mouse                = "org.typelevel" %% "mouse" % Version.mouse
  private val shapeless            = "com.chuusai" %% "shapeless" % Version.shapeless
  private val typesafeConfig       = "com.typesafe" % "config" % Version.typesafeConfig
  private val scopt                = "com.github.scopt" %% "scopt" % Version.scopt
  private val ficus                = "com.iheart" %% "ficus" % Version.ficus
  private val logback              = "ch.qos.logback" % "logback-classic" % Version.logback
  private val logbackJsonEncoder   = "net.logstash.logback" % "logstash-logback-encoder" % Version.logbackJsonEncoder
  private val slf4j                = "org.slf4j" % "slf4j-api" % Version.slf4j
  private val julToSlf4j           = "org.slf4j" % "jul-to-slf4j" % Version.slf4j
  private val janino               = "org.codehaus.janino" % "janino" % Version.janino
  private val kamonCore            = kamonModule("core", Version.kamonCore)
  private val wavesProtobufSchemas = ("com.wavesplatform" % "protobuf-schemas" % Version.wavesProtobufSchemas classifier "proto") % "protobuf" // for teamcity
  private val wavesJ = "com.wavesplatform" % "wavesj" % Version.wavesJ excludeAll (
    // Conflicts with specified gRPC. This is the problem for waves-integration-it.
    // Also, wavesj doesn't use gRPC, so it is safe.
    ExclusionRule(organization = "io.grpc"),
    ExclusionRule("com.wavesplatform", "protobuf-schemas")
  )
  private val toxiProxy     = "org.testcontainers" % "toxiproxy" % Version.testContainersToxiProxy
  private val googleGuava   = "com.google.guava" % "guava" % Version.googleGuava
  private val kafka         = "org.apache.kafka" % "kafka-clients" % Version.kafka
  private val grpcNetty     = "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion
  private val swagger       = "com.github.swagger-akka-http" %% "swagger-akka-http" % Version.swagger
  private val swaggerUi     = "org.webjars" % "swagger-ui" % Version.swaggerUi
  private val playJson      = "com.typesafe.play" %% "play-json" % Version.playJson
  private val scorexCrypto  = "org.scorexfoundation" %% "scrypto" % Version.scorexCrypto
  private val grpcScalaPb   = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
  private val monixReactive = monixModule("reactive")
  private val supertagged   = "org.rudogma" %% "supertagged" % Version.supertagged
  private val javaLevelDb   = "org.iq80.leveldb" % "leveldb" % Version.javaLevelDb
  private val jniLevelDb    = "org.ethereum" % "leveldbjni-all" % Version.jniLevelDb
  private val influxDb      = "org.influxdb" % "influxdb-java" % Version.influxDb
  private val commonsNet    = "commons-net" % "commons-net" % Version.commonsNet

  private val silencer: Seq[ModuleID] = Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % Version.silencer cross CrossVersion.full),
    "com.github.ghik" %% "silencer-lib" % Version.silencer % Provided cross CrossVersion.full
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
    akkaModule("akka-http-testkit", Version.akkaHttp),
    scalaTest,
    scalaCheck,
    scalaTestPlusCheck,
    scalaMock,
    javaLevelDb
  ) map (_ % Test)

  private val integrationTestKit: Seq[ModuleID] = Seq(wavesJ, logback % Test) ++ testKit ++ silencer

  val globalEnforcedVersions = Def.setting(
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
      catsModule("kernel"),
      catsModule("macros"),
      catsCore,
      shapeless,
      kamonCore,
      typesafeConfig,
      scalaTest % Test,
      googleGuava,
      slf4j,
      grpcNetty,
      "io.netty" % "netty-codec-http2" % "4.1.33.Final"
    )
  )

  object Module {

    lazy val dex: Seq[ModuleID] = Seq(
      akkaActor,
      akkaHttp,
      akkaModule("akka-slf4j", Version.akka),
      julToSlf4j,
      logback,
      logbackJsonEncoder % Runtime,
      kindProjector,
      catsTaglessMacros,
      shapeless,
      mouse,
      scopt,
      kafka,
      janino,
      jniLevelDb,
      kamonCore,
      kamonModule("influxdb", Version.kamonInfluxDb),
      kamonModule("system-metrics", Version.kamonSystemMetrics),
      influxDb,
      commonsNet,
      swaggerUi
    ) ++ testKit ++ quill ++ silencer

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
      toxiProxy,
      wavesJ
    ) ++ testContainers

    lazy val dexTestCommon: Seq[ModuleID] = Seq(diffx, scalaTest, scalaCheck, scalaTestPlusCheck)

    lazy val wavesExt: Seq[ModuleID] = Seq(
      julToSlf4j,
      grpcNetty,
      grpcScalaPb
    )

    lazy val wavesGrpc: Seq[ModuleID] = Seq(wavesProtobufSchemas, grpcScalaPb) ++ silencer

    lazy val wavesIntegration: Seq[ModuleID] = Seq(
      julToSlf4j,
      logback,
      swagger,
      playJson,
      ficus,
      scorexCrypto,
      catsCore,
      supertagged,
      monixReactive,
      betterMonadicFor,
      mouse,
      grpcNetty
    ) ++ testKit

    lazy val wavesIntegrationIt: Seq[ModuleID] = Seq(
      julToSlf4j
    ) ++ integrationTestKit
  }
}
