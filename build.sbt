import CommonSettings.autoImport.network
import ReleasePlugin.autoImport._
import sbt.Keys._
import sbt._
import sbt.internal.inc.ReflectUtilities

// Scalafix
scalafixDependencies in ThisBuild += "org.scalatest" %% "autofix" % "3.1.0.0"
addCompilerPlugin(scalafixSemanticdb)

lazy val commonOwaspSettings = Seq(
  dependencyCheckAssemblyAnalyzerEnabled := Some(false)
)

// Used in unit and integration tests
lazy val `dex-test-common` = project.settings(commonOwaspSettings).dependsOn(`waves-integration`)

lazy val dex = project
  .settings(commonOwaspSettings)
  .dependsOn(
    `waves-integration`,
    `dex-test-common` % "test->compile"
  )

lazy val `dex-it-common` = project
  .settings(commonOwaspSettings)
  .dependsOn(
    dex % "compile;runtime->provided",
    `dex-test-common`
  )

lazy val `dex-it` = project
  .settings(commonOwaspSettings)
  .dependsOn(
    dex % "compile;test->test",
    `waves-integration-it`,
    `dex-it-common`
  )

lazy val `waves-grpc` = project.settings(commonOwaspSettings)

lazy val `waves-ext` = project
  .settings(commonOwaspSettings)
  .dependsOn(
    `waves-grpc`,
    `dex-test-common` % "test->compile"
  )

lazy val `waves-integration` = project.settings(commonOwaspSettings).dependsOn(`waves-grpc`)

lazy val `waves-integration-it` = project
  .settings(commonOwaspSettings)
  .dependsOn(
    `waves-integration`,
    `dex-it-common`
  )

lazy val `dex-jmh` = project.dependsOn(dex % "compile;test->test")

lazy val it = project
  .settings(
    description := "Hack for near future to support builds in TeamCity for old and new branches both",
    Test / test := Def
      .sequential(
        root / Compile / packageAll,
        Def.task {
          val wavesIntegrationDocker = (`waves-integration-it` / Docker / docker).value
          val dexDocker              = (`dex-it` / Docker / docker).value
        },
        `waves-integration-it` / Test / test,
        `dex-it` / Test / test
      )
      .value
  )

lazy val root = (project in file("."))
  .settings(name := "dex-root")
  .settings(commonOwaspSettings)
  .aggregate(
    `dex-test-common`,
    dex,
    `dex-it-common`,
    `dex-it`,
    `waves-grpc`,
    `waves-ext`,
    `waves-integration`,
    `waves-integration-it`,
    `dex-jmh`
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.10",
    organization := "com.wavesplatform",
    organizationName := "Waves Platform",
    organizationHomepage := Some(url("https://wavesplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/wavesplatform/dex"), "git@github.com:wavesplatform/dex.git", None)),
    licenses := Seq(("MIT", url("https://github.com/wavesplatform/dex/blob/master/LICENSE"))),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ypartial-unification",
      "-opt:l:inline",
      "-opt-inline-from:**",
      "-Yrangepos" // required for scalafix
    ),
    crossPaths := false,
    scalafmtOnCompile := false,
    dependencyOverrides ++= Dependencies.globalEnforcedVersions.value,
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    /* http://www.scalatest.org/user_guide/using_the_runner
     * o - select the standard output reporter
     * I - show reminder of failed and canceled tests without stack traces
     * D - show all durations
     * O - drop InfoProvided events
     * F - show full stack traces
     * u - select the JUnit XML reporter with output directory
     */
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    },
    network := NodeNetwork(sys.props.get("network")),
    // To speedup the compilation
    Compile / doc / sources := Seq.empty,
    Compile / packageDoc / publishArtifact := false
  )
)

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// root project settings
enablePlugins(ReleasePlugin)

// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList.map(x => x: ProjectReference)

Compile / cleanAll := {
  val xs = allProjects
  streams.value.log.info(s"Cleaning ${xs.mkString(", ")}")
  clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile, Test))).value
}

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := {
  // try/finally is a hack to run clean before all tasks
  try {
    (root / Compile / cleanAll).value
  } finally {
    (dex / Test / test).value
    (`waves-ext` / Test / test).value
    (`waves-integration` / Test / test).value
    (`dex-jmh` / Test / compile).value
  }
}

commands += Command.command("checkPR") { state =>
  val updatedState =
    Project
      .extract(state)
      .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)
  Project.extract(updatedState).runTask(root / checkPRRaw, updatedState)
  state
}

// IDE settings
ideExcludedDirectories := Seq((root / baseDirectory).value / "_local")
