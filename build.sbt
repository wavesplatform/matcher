import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import CommonSettings.autoImport.network
import Hashes.mk
import org.apache.commons.codec.binary.Hex
import sbt.Keys._
import sbt._
import sbt.internal.inc.ReflectUtilities
import sbt.io.Using

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

def nodeVersionTag: String = "v1.0.1"

lazy val node = ProjectRef(uri(s"git://github.com/wavesplatform/Waves.git#$nodeVersionTag"), "node")

lazy val `node-it` = ProjectRef(uri(s"git://github.com/wavesplatform/Waves.git#$nodeVersionTag"), "node-it")

lazy val dex = project.dependsOn(node % "compile;test->test;runtime->provided")

lazy val `dex-it` = project
  .dependsOn(
    dex,
    `node-it` % "compile;test->test"
  )

lazy val `dex-generator` = project.dependsOn(
  dex,
  `node-it` % "compile->test", // Without this IDEA doesn't find classes
  `dex-it`  % "compile->test"
)

lazy val it = project
  .settings(
    description := "Hack for near future to support builds in TeamCity for old and new branches both",
    Test / test := Def
      .sequential(
        root / packageAll,
        `dex-it` / Docker / docker,
        `dex-it` / Test / test
      )
      .value
  )

lazy val root = (project in file("."))
  .aggregate(
    dex,
    `dex-it`,
    `dex-generator`
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.8",
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
      "-opt-inline-from:**"
    ),
    crossPaths := false,
    scalafmtOnCompile := false,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
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
    nodeVersion := (node / version).value,
    buildNodeContainer := (`node-it` / Docker / docker).value
  ))

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// root project settings
// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList map { p =>
  p: ProjectReference
}

lazy val cleanAll = taskKey[Unit]("Clean all projects")
cleanAll := clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile))).value

lazy val releaseDirectory = settingKey[Path]("Directory for release artifacts")
releaseDirectory := ((root / target).value / "release").toPath

lazy val genDocs = taskKey[Unit]("Generates the documentation")
genDocs := Def.taskDyn {
  val configFile = (root / baseDirectory).value / "_local" / "mainnet.sample.conf" // Actually doesn't matter for this task
  streams.value.log.info(s"${configFile.getAbsolutePath} gen-docs ${releaseDirectory.value}")
  (dex / Compile / runMain).toTask(s" com.wavesplatform.dex.MatcherTool ${configFile.getAbsolutePath} gen-docs ${releaseDirectory.value}")
}.value

lazy val release = taskKey[Unit]("Packages artifacts and writes a documentation to the releaseDirectory")
release := Def
  .sequential(
    root / cleanAll,
    Def.task { Files.createDirectories(releaseDirectory.value) },
    Def.task { Command.process("packageAllForNetworks", state.value) },
    genDocs,
    writeReleaseNotes
  )
  .value

lazy val packageAll = taskKey[Unit]("Package all artifacts")
packageAll := Def
  .sequential(
    Def.task {
      val artifacts = Seq(
        (dex / Universal / packageZipTarball).value,
        (dex / Debian / packageBin).value
      )

      val destDir = releaseDirectory.value
      artifacts
        .map(_.toPath)
        .foreach { source =>
          Files.move(source, destDir.resolve(source.getFileName))
        }
    }
  )
  .value

lazy val artifactExtensions = settingKey[Seq[String]]("Used to search artifacts")
artifactExtensions := List("deb", "tgz")

lazy val writeReleaseNotes = taskKey[Unit](s"""1. Collects commits between last two tags
     |2. Writes a draft of a release to the releaseDirectory
     |3. Adds checksums for artifacts in the releaseDirectory""".stripMargin)
writeReleaseNotes := {
  val runner           = git.runner.value
  val log              = streams.value.log
  val cwd              = (root / baseDirectory).value
  val destDir          = (root / releaseDirectory).value
  val releaseNotesFile = destDir.resolve("release-notes.md").toFile
  val artifactsFilter  = artifactExtensions.value.foldLeft[FileFilter](NothingFilter)((r, x) => r || GlobFilter(s"*.$x"))

  val tags = runner("tag", "--sort", "version:refname")(cwd, sbt.Logger.Null).replaceAll("\r", "").split("\n")
  val changesContent = if (tags.length <= 1) {
    log.warn(s"Expected at least two tags, but there is ${tags.length}")
    ""
  } else {
    val lastTag = tags.last
    val prevTag = tags(tags.length - 2)
    log.info(s"Looking for commits between $prevTag and $lastTag")

    runner("log", "--pretty=%an%n%B%n", s"$prevTag..$lastTag")(cwd, sbt.Logger.Null).replaceAll("\r", "").replaceAll("\n{3,}", "\n").trim
  }

  val artifacts = IO.listFiles(destDir.toFile, artifactsFilter)
  log.info(s"Found artifacts: ${artifacts.map(_.getName).mkString(", ")}")

  val hashes = artifacts.map { file =>
    val xs = Using.fileInputStream(file)(mk("SHA-256", _))
    file.toPath.getFileName.toString -> Hex.encodeHexString(xs)
  }

  val sortedHashes = hashes.sortBy { case (fileName, _) => (fileName.length, fileName) }
  val hashesContent =
    s"""## SHA256 Checksums
       |
       |```
       |${sortedHashes.map { case (fileName, hash) => s"$hash $fileName" }.mkString("\n")}
       |```
       |""".stripMargin

  val content = s"""$changesContent
                   |
                   |$hashesContent""".stripMargin

  log.info(s"Write release notes to $releaseNotesFile")
  IO.write(releaseNotesFile, content, StandardCharsets.UTF_8)
}

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := {
  try {
    cleanAll.value // Hack to run clean before all tasks
  } finally {
    (dex / Test / test).value
    (`dex-generator` / Test / compile).value
  }
}

def checkPR: Command = Command.command("checkPR") { state =>
  val updatedState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)
  Project.extract(updatedState).runTask(root / checkPRRaw, updatedState)
  state
}

def packageAllForNetworks: Command = Command.command("packageAllForNetworks") { state =>
  val testNetState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / network := Testnet), state)

  Project.extract(state).runTask(root / packageAll, state)
  Project.extract(testNetState).runTask(root / packageAll, testNetState)
  state
}

commands ++= Seq(checkPR, packageAllForNetworks)

// IDE settings
ideExcludedDirectories := Seq((root / baseDirectory).value / "_local")
