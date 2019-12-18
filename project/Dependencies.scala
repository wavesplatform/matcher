import sbt.Keys._
import sbt._

object Dependencies {

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  private def akkaHttpModule(module: String)               = "com.typesafe.akka"             %% module            % "10.1.8"
  private def nettyModule(module: String)                  = "io.netty"                      % s"netty-$module"   % "4.1.33.Final"
  private def kamonModule(module: String, v: String)       = "io.kamon"                      %% s"kamon-$module"  % v
  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.9.8"
  private def bouncyCastle(module: String)                 = "org.bouncycastle"              % s"$module-jdk15on" % "1.59"

  private def catsModule(module: String, version: String = "1.6.0") = Def.setting("org.typelevel" %% s"cats-$module" % version)

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

  val akkaHttp                   = akkaHttpModule("akka-http")
  private val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  private val googleGuava        = "com.google.guava" % "guava" % "27.0.1-jre"
  private val kamonCore          = kamonModule("core", "1.1.5")
  private val machinist          = "org.typelevel" %% "machinist" % "0.6.6"
  private val logback            = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val janino                     = "org.codehaus.janino" % "janino" % "3.0.12"
  val mouse                      = "org.typelevel" %% "mouse" % "0.22"

  private val catsEffect = catsModule("effect", "1.2.0")
  private val catsCore   = catsModule("core")
  private val shapeless  = Def.setting("com.chuusai" %% "shapeless" % "2.3.3")

  private val quill = Seq(
    "org.postgresql" % "postgresql"  % "9.4.1208",
    "io.getquill"    %% "quill-jdbc" % "3.1.0"
  )

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.6" % Test

  val enforcedVersions = Def.setting(
    Seq(
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      jacksonModuleScala,
      scalaTest,
      googleGuava,
      "org.slf4j" % "slf4j-api" % "1.7.25",
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      kamonCore,
      "com.typesafe" % "config" % "1.3.3",
      machinist,
      "com.squareup.okhttp3" % "okhttp"      % "3.11.0",
      "com.squareup.okio"    % "okio"        % "1.14.0",
      "com.lihaoyi"          %% "sourcecode" % "0.1.4",
      nettyModule("handler"),
      bouncyCastle("bcpkix"),
      bouncyCastle("bcprov"),
      "org.apache.httpcomponents" % "httpcore"         % "4.4.5",
      "org.javassist"             % "javassist"        % "3.21.0-GA",
      "org.reactivestreams"       % "reactive-streams" % "1.0.2",
      "org.scala-lang"            % "scala-library"    % scalaVersion.value,
      "org.scala-lang"            % "scala-reflect"    % scalaVersion.value,
      catsEffect.value,
      catsCore.value,
      catsModule("kernel").value,
      catsModule("macros").value,
      shapeless.value
    ))

  val console = Seq("com.github.scopt" %% "scopt" % "4.0.0-RC2")

  lazy val itTest = scalaTest +: Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.15.1").classifier("shaded"),
    jacksonModule("dataformat", "dataformat-properties"),
    "org.asynchttpclient" % "async-http-client" % "2.7.0",
    "org.scalacheck"      %% "scalacheck"       % "1.14.0"
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback.exclude("org.scala-js", "scalajs-library_2.12"),
    "org.scalacheck" %% "scalacheck" % "1.14.0",
    ("io.github.amrhassan" %% "scalacheck-cats" % "0.4.0").exclude("org.scalacheck", "scalacheck_2.12"),
    "org.mockito"   % "mockito-all"                  % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0"
  ).map(_ % Test)

  lazy val dex =
    Seq(
      logback.exclude("org.scala-js", "scalajs-library_2.12"),
      kindProjector,
      akkaModule("actor"),
      akkaModule("persistence-query"),
      akkaModule("slf4j"),
      akkaHttp,
      "org.apache.kafka" % "kafka-clients" % "2.3.1",
      // "javax.xml.bind" % "jaxb-api" % "2.3.1", // javax.xml.bind replacement for jackson in swagger, will required in future
      janino,
      mouse
    ) ++ Seq(
      akkaModule("testkit"),
      akkaModule("persistence-tck"),
      "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.1"
    ).map(_ % Test) ++ test ++ quill
}
