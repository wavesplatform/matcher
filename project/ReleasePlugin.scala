import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import CommonSettings.autoImport.network
import Hashes.mk
import com.typesafe.sbt.GitPlugin.autoImport.git
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.Debian
import com.typesafe.sbt.packager.debian.JDebPackaging
import com.typesafe.sbt.packager.universal.UniversalDeployPlugin
import org.apache.commons.codec.binary.Hex
import sbt.Keys._
import sbt._
import sbt.io.Using

object ReleasePlugin extends AutoPlugin {

  object autoImport extends ReleasePluginKeys
  import autoImport._

  override def requires: Plugins = CommonSettings && UniversalDeployPlugin && JDebPackaging

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    releaseDirectory := (target.value / "release").toPath,
    artifactExtensions := List("deb", "tgz"),
    release := Def
      .sequential(
        cleanAll,
        Def.task { Files.createDirectories(releaseDirectory.value) },
        Def.task { Command.process("packageAllForNetworks", state.value) },
        genDocs,
        writeReleaseNotes
      )
      .value,
    commands += Command.command("packageAllForNetworks") { state =>
      val testNetState = Project
        .extract(state)
        .appendWithoutSession(Seq(Global / network := Testnet), state)

      Project.extract(state).runTask(packageAll, state)
      Project.extract(testNetState).runTask(packageAll, testNetState)
      state
    },
    packageAll := Def
      .sequential(
        Def.task {
          // Don't forget to update "artifactExtensions" if there is a new artifact
          val artifacts = Seq(
            (LocalProject("dex") / Universal / packageZipTarball).value,
            (LocalProject("dex") / Debian / packageBin).value
          )

          val destDir = releaseDirectory.value
          artifacts
            .map(_.toPath)
            .foreach { source =>
              Files.move(source, destDir.resolve(source.getFileName))
            }
        }
      )
      .value,
    genDocs := Def.taskDyn {
      val configFile = baseDirectory.value / "_local" / "mainnet.sample.conf" // Actually doesn't matter for this task
      streams.value.log.info(s"${configFile.getAbsolutePath} gen-docs ${releaseDirectory.value}")
      (LocalProject("dex") / Compile / runMain)
        .toTask(s" com.wavesplatform.dex.MatcherTool ${configFile.getAbsolutePath} gen-docs ${releaseDirectory.value}")
    }.value,
    writeReleaseNotes := {
      val runner           = git.runner.value
      val log              = streams.value.log
      val cwd              = baseDirectory.value
      val destDir          = releaseDirectory.value
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
    },
    cleanAll := List.empty
  )
}

trait ReleasePluginKeys {
  val releaseDirectory   = settingKey[Path]("Directory for release artifacts")
  val artifactExtensions = settingKey[Seq[String]]("Used to search artifacts")

  val release           = taskKey[Unit]("Packages artifacts and writes a documentation to the releaseDirectory")
  val cleanAll          = taskKey[Unit]("Clean all projects")
  val packageAll        = taskKey[Unit]("Package all artifacts")
  val genDocs           = taskKey[Unit]("Generates the documentation")
  val writeReleaseNotes = taskKey[Unit](s"""1. Collects commits between last two tags
                                           |2. Writes a draft of a release to the releaseDirectory
                                           |3. Adds checksums for artifacts in the releaseDirectory""".stripMargin)
}
