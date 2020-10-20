import sbt.Keys._
import sbt.{Def, compilerPlugin, _}

object Dependencies {

  object Version {

    val parCollections = "0.2.0"

    val akka = "2.6.8"
    val akkaHttp = "10.2.0"

    val scalaTest = "3.2.2"
    val scalaCheck = "1.14.3"
    val scalaTestPlusCheck = "3.2.2.0"
    val scalaMock = "4.4.0"
    val diffx = "0.3.29"

    val cats = "2.1.1"
    val catsTaglessMacros = "0.11"
    val kindProjector = "0.9.10"
    val betterMonadicFor = "0.3.1"
    val mouse = "0.25"
    val shapeless = "2.3.3"
    val monocle = "2.1.0"

    val typesafeConfig = "1.4.0"
    val scopt = "4.0.0-RC2"

    val logback = "1.2.3"
    val slf4j = "1.7.30"
    val janino = "3.1.2"
    val logbackJsonEncoder = "6.4"

    val silencer = "1.7.1"

    val kamonCore = "2.1.6"
    val kamonInfluxDb = "2.1.6"

    val wavesProtobufSchemas = "1.2.6"
    val wavesJ = "1.0.1"

    val postgresql = "42.2.16"
    val quillJdbc = "3.5.2"

    val sttp = "1.7.2"
    val sttpClient = "2.2.7"

    val testContainers = "0.38.1"
    val testContainersPostgres = "1.14.3"
    val testContainersKafka = "1.14.3"
    val testContainersToxiProxy = "1.14.3"

    val jackson = "2.10.0"
    val playJson = "2.9.0"

    val googleGuava = "28.2-jre"
    val kafka = "2.6.0"

    val swagger = "1.1.2"
    val swaggerUi = "3.32.5"
    val jaxbApi = "2.3.1"

    val scorexCrypto = "2.1.9"

    val monix = "3.2.2"

    val supertagged = "1.5"

    val javaLevelDb = "0.12"
    val jniLevelDb = "1.18.3"
    val influxDb = "2.20"

    val commonsNet = "3.7"
    val nettyCodec = "4.1.33.Final"
    val jwt = "4.3.0"

    val pureConfig = "0.14.0"
    val allureScalaTest = "2.13.5"
    val enumeratum = "1.6.1"
  }

  private def akkaModule(module: String, version: String): ModuleID = "com.typesafe.akka" %% module % version
  private def scalaModule(module: String, version: String): ModuleID = "org.scala-lang" % module % version
  private def catsModule(module: String): ModuleID = "org.typelevel" %% s"cats-$module" % Version.cats
  private def sttpModule(module: String): ModuleID = "com.softwaremill.sttp" %% module % Version.sttp
  private def jacksonModule(group: String, module: String): ModuleID = s"com.fasterxml.jackson.$group" % s"jackson-$module" % Version.jackson
  private def monixModule(module: String): ModuleID = "io.monix" %% s"monix-$module" % Version.monix
  private def kamonModule(module: String, version: String): ModuleID = "io.kamon" %% s"kamon-$module" % version
  private def jwtModule(module: String): ModuleID = "com.pauldijou" %% s"jwt-$module" % Version.jwt

  private val parCollections = "org.scala-lang.modules" %% "scala-parallel-collections" % Version.parCollections
  private val akkaActor = akkaModule("akka-actor", Version.akka)
  private val akkaActorTyped = akkaModule("akka-actor-typed", Version.akka)
  private val akkaStreamsTyped = akkaModule("akka-stream-typed", Version.akka)
  private val akkaHttp = akkaModule("akka-http", Version.akkaHttp)
  private val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % Version.scalaCheck
  private val scalaTestPlusCheck = "org.scalatestplus" %% "scalacheck-1-14" % Version.scalaTestPlusCheck
  private val scalaMock = "org.scalamock" %% "scalamock" % Version.scalaMock
  private val diffx = "com.softwaremill.diffx" %% "diffx-scalatest" % Version.diffx
  private val catsCore = catsModule("core")
  private val catsTaglessMacros = "org.typelevel" %% "cats-tagless-macros" % Version.catsTaglessMacros
  private val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  private val betterMonadicFor = compilerPlugin("com.olegpy" %% "better-monadic-for" % Version.betterMonadicFor)
  private val mouse = "org.typelevel" %% "mouse" % Version.mouse
  private val shapeless = "com.chuusai" %% "shapeless" % Version.shapeless
  private val typesafeConfig = "com.typesafe" % "config" % Version.typesafeConfig
  private val scopt = "com.github.scopt" %% "scopt" % Version.scopt
  private val logback = "ch.qos.logback" % "logback-classic" % Version.logback
  private val logbackJsonEncoder = "net.logstash.logback" % "logstash-logback-encoder" % Version.logbackJsonEncoder
  private val slf4j = "org.slf4j" %% "slf4j-api" % Version.slf4j
  private val julToSlf4j = "org.slf4j" % "jul-to-slf4j" % Version.slf4j
  private val janino = "org.codehaus.janino" % "janino" % Version.janino
  private val kamonCore = kamonModule("core", Version.kamonCore)

  private val wavesProtobufSchemas =
    ("com.wavesplatform" % "protobuf-schemas" % Version.wavesProtobufSchemas classifier "proto") % "protobuf" // for teamcity

  private val wavesJ = "com.wavesplatform" % "wavesj" % Version.wavesJ excludeAll (
    // Conflicts with specified gRPC. This is the problem for waves-integration-it.
    // Also, wavesj doesn't use gRPC, so it is safe.
    ExclusionRule(organization = "io.grpc")
  )

  private val toxiProxy = "org.testcontainers" % "toxiproxy" % Version.testContainersToxiProxy
  private val googleGuava = "com.google.guava" % "guava" % Version.googleGuava
  private val kafka = "org.apache.kafka" % "kafka-clients" % Version.kafka
  private val grpcNetty = "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion
  private val nettyCodec = "io.netty" % "netty-codec-http2" % Version.nettyCodec
  private val swagger = "com.github.swagger-akka-http" %% "swagger-akka-http" % Version.swagger
  private val swaggerUi = "org.webjars" % "swagger-ui" % Version.swaggerUi
  private val playJson = "com.typesafe.play" %% "play-json" % Version.playJson
  private val scorexCrypto = "org.scorexfoundation" %% "scrypto" % Version.scorexCrypto
  private val grpcScalaPb = "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
  private val monixReactive = monixModule("reactive")
  private val supertagged = "org.rudogma" %% "supertagged" % Version.supertagged
  private val javaLevelDb = "org.iq80.leveldb" % "leveldb" % Version.javaLevelDb
  private val jniLevelDb = "org.ethereum" % "leveldbjni-all" % Version.jniLevelDb
  private val influxDb = "org.influxdb" % "influxdb-java" % Version.influxDb
  private val commonsNet = "commons-net" % "commons-net" % Version.commonsNet
  private val sttpClient = "com.softwaremill.sttp.client" %% "core" % Version.sttpClient
  private val allureScalaTest = "io.qameta.allure" %% "allure-scalatest" % Version.allureScalaTest
  private val jaxbApi = "javax.xml.bind" % "jaxb-api" % Version.jaxbApi

  private val pureConfig: Seq[ModuleID] =
    Seq("pureconfig", "pureconfig-cats", "pureconfig-enumeratum").map("com.github.pureconfig" %% _ % Version.pureConfig)

  private val enumeratum: Seq[ModuleID] = Seq("enumeratum", "enumeratum-play-json").map("com.beachape" %% _ % Version.enumeratum)

  private val monocle: Seq[ModuleID] = Seq("monocle-core", "monocle-macro").map("com.github.julien-truffaut" %% _ % Version.monocle)

  private val silencer: Seq[ModuleID] = Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % Version.silencer cross CrossVersion.full),
    "com.github.ghik" %% "silencer-lib" % Version.silencer % Provided cross CrossVersion.full
  )

  private val quill: Seq[ModuleID] = Seq(
    "org.postgresql" % "postgresql" % Version.postgresql,
    "io.getquill" %% "quill-jdbc" % Version.quillJdbc
  )

  private val testContainers: Seq[ModuleID] = Seq(
    "com.dimafeng" %% "testcontainers-scala" % Version.testContainers,
    "org.testcontainers" % "postgresql" % Version.testContainersPostgres,
    "org.testcontainers" % "kafka" % Version.testContainersKafka
  )

  private val testKit: Seq[ModuleID] = Seq(
    akkaModule("akka-testkit", Version.akka),
    akkaModule("akka-http-testkit", Version.akkaHttp),
    akkaModule("akka-actor-testkit-typed", Version.akka),
    scalaTest,
    scalaCheck,
    scalaTestPlusCheck,
    scalaMock,
    javaLevelDb,
    allureScalaTest
  ) map (_ % Test)

  private val integrationTestKit: Seq[ModuleID] = Seq(wavesJ, logback % Test) ++ testKit

  val globalEnforcedVersions = Def.setting(
    Seq(
      akkaActor,
      akkaActorTyped,
      akkaStreamsTyped,
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
      nettyCodec
    ) ++ pureConfig ++ enumeratum
  )

  object Module {

    lazy val dex: Seq[ModuleID] = Seq(
      akkaActor,
      akkaActorTyped,
      akkaStreamsTyped,
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
      influxDb,
      commonsNet,
      swaggerUi,
      jaxbApi, // Required for Swagger UI in JRE 11 because of javax/xml/bind/annotation/XmlRootElement
      jwtModule("core"),
      jwtModule("play-json"),
      sttpClient,
      wavesJ,
      betterMonadicFor
    ) ++ pureConfig ++ enumeratum ++ testKit ++ quill ++ monocle

    lazy val dexLoad: Seq[ModuleID] = Seq(diffx) ++ pureConfig

    lazy val dexIt: Seq[ModuleID] = integrationTestKit ++ Seq(parCollections)

    lazy val dexItCommon: Seq[ModuleID] = Seq(
      kindProjector,
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

    lazy val dexTestCommon: Seq[ModuleID] = Seq(diffx, scalaTest, scalaCheck, scalaTestPlusCheck, allureScalaTest)

    lazy val wavesExt: Seq[ModuleID] = Seq(
      grpcNetty
    )

    lazy val wavesGrpc: Seq[ModuleID] = Seq(wavesProtobufSchemas, grpcScalaPb) ++ silencer

    lazy val wavesIntegration: Seq[ModuleID] = Seq(
      julToSlf4j,
      logback,
      swagger,
      playJson,
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
