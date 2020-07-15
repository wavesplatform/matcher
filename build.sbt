import CommonSettings.autoImport.network
import ReleasePlugin.autoImport._
import sbt.Keys._
import sbt._
import sbt.internal.inc.ReflectUtilities

Global / resolvers += Resolver.bintrayRepo("ethereum", "maven") // JNI LevelDB

// Scalafix
scalafixDependencies in ThisBuild ++= List(
  "org.scalatest"          %% "autofix"                     % "3.1.0.0",
  "org.scala-lang.modules" %% "scala-collection-migrations" % "2.1.4"
)
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

lazy val `dex-load` = project
  .settings(commonOwaspSettings)
  .dependsOn(
    dex,
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
        root / Compile / cleanAll,
        Def.task {
          Command.process("fullCheck", state.value)
        }
      )
      .value
  )

lazy val root = (project in file("."))
  .settings(name := "dex-root")
  .settings(commonOwaspSettings)
  .aggregate(
    dex,
    `dex-it`,
    `dex-load`,
    `dex-it-common`,
    `dex-jmh`,
    `dex-test-common`,
    `waves-ext`,
    `waves-grpc`,
    `waves-integration`,
    `waves-integration-it`
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.13.2",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
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
      "-Ywarn-macros:after", // https://github.com/scala/bug/issues/11099
      "-Xlint",
      "-opt:l:inline",
      "-opt-inline-from:**",
      "-Yrangepos", // required for scalafix
      "-P:semanticdb:synthetics:on"
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

// FIX https://github.com/sbt/sbt-git/issues/161#issuecomment-469342173
git.gitDescribedVersion := git.gitDescribedVersion { _ =>
  import scala.sys.process._
  val nativeGitDescribeResult = s"git describe --tags".!!.trim
  git.defaultTagByVersionStrategy(nativeGitDescribeResult)
}.value

// root project settings
enablePlugins(ReleasePlugin)

// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList.map(x => x: ProjectReference)

Compile / cleanAll := {
  val xs = allProjects
  streams.value.log.info(s"Cleaning ${xs.mkString(", ")}")
  clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile, Test))).value
}

lazy val quickCheckRaw = taskKey[Unit]("Build a project and run unit tests")
quickCheckRaw := Def
  .sequential(
    root / Test / compile,
    `waves-ext` / Test / test,
    `waves-integration` / Test / test,
    dex / Test / test,
    root / Compile / packageAll
  )
  .value

lazy val fullCheckRaw = taskKey[Unit]("Build a project and run all tests")
fullCheckRaw := Def
  .sequential(
    quickCheckRaw,
    Def.task {
      val wavesIntegrationDocker = (`waves-integration-it` / Docker / docker).value
      val dexDocker              = (`dex-it` / Docker / docker).value
    },
    `waves-integration-it` / Test / test,
    `dex-it` / Test / test
  )
  .value

def mkCheckCommand(name: String, task: TaskKey[Unit]): Command = Command.command(name) { state =>
  val updatedState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)

  Project.extract(updatedState).runTask(task, updatedState)
  state
}

commands ++= List(
  mkCheckCommand("quickCheck", root / quickCheckRaw),
  mkCheckCommand("fullCheck", root / fullCheckRaw)
)

// IDE settings
ideExcludedDirectories := Seq((root / baseDirectory).value / "_local")
