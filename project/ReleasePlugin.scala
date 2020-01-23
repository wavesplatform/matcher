import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import CommonSettings.autoImport.network
import Hashes.mk
import com.typesafe.sbt.GitPlugin.autoImport.git
import com.typesafe.sbt.SbtNativePackager.Universal
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

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(
      Seq(
        releaseDirectory := ((Compile / target).value / "release").toPath,
        artifactExtensions := List("deb", "tgz"),
        release := Def
          .sequential(
            Compile / cleanAll,
            Compile / createDirectories,
            Def.task { Command.process("packageAllForNetworks", (Compile / state).value) },
            Compile / genDocs,
            Compile / writeReleaseNotes
          )
          .value,
        packageAll := Def
          .sequential(
            Compile / createDirectories,
            Def.task {
              // Don't forget to update "artifactExtensions" if there is a new artifact
              val artifacts = Seq(
                (LocalProject("waves-ext") / Universal / packageBin).value,
                (LocalProject("waves-ext") / Debian / packageBin).value,
                (LocalProject("dex") / Universal / packageBin).value,
                (LocalProject("dex") / Debian / packageBin).value
              )

              val destDir = (Compile / releaseDirectory).value
              artifacts
                .map(_.toPath)
                .foreach { source =>
                  val dest = destDir.resolve(source.getFileName)
                  if (!dest.toFile.isFile) Files.move(source, dest)
                }
            }
          )
          .value,
        genDocs := Def.taskDyn {
          val outputDirectory = (Compile / releaseDirectory).value
          streams.value.log.info(s"Saving documentation to $outputDirectory")
          (LocalProject("dex") / Compile / runMain)
            .toTask(s" com.wavesplatform.dex.WavesDexCli create-documentation --output-directory $outputDirectory")
        }.value,
        writeReleaseNotes := {
          val runner           = git.runner.value
          val log              = streams.value.log
          val cwd              = (Compile / baseDirectory).value
          val destDir          = (Compile / releaseDirectory).value
          val releaseNotesFile = destDir.resolve("release-notes.md").toFile
          val artifactsFilter  = (Compile / artifactExtensions).value.foldLeft[FileFilter](NothingFilter)((r, x) => r || GlobFilter(s"*.$x"))

          val tags = runner("tag", "--sort", "version:refname")(cwd, sbt.Logger.Null).replaceAll("\r", "").split("\n")
          val (prevTag, lastTag) = if (tags.length <= 1) {
            log.warn(s"Expected at least two tags, but there is ${tags.length}: ${tags.mkString(", ")}.")
            (tags.headOption.getOrElse("HEAD"), "HEAD")
          } else {
            val lastTag = tags.last
            val prevTag = tags(tags.length - 2)

            val lastTagCommitId = runner("rev-parse", "--short", lastTag)(cwd, sbt.Logger.Null)
            val lastCommitId    = runner("rev-parse", "--short", "HEAD")(cwd, sbt.Logger.Null)
            if (lastTagCommitId != lastCommitId)
              log.warn(s"Dirty release! The last tag ($lastTag) commit id is: $lastTagCommitId, last commit id is: $lastCommitId")

            (prevTag, lastTag)
          }

          log.info(s"Looking for commits between $prevTag and $lastTag")
          val changesContent =
            runner("log", "--pretty=%an%n%B%n", s"$prevTag..$lastTag")(cwd, sbt.Logger.Null)
              .replaceAll("\r", "")
              .replaceAll("\n{3,}", "\n")
              .trim

          val artifacts = IO.listFiles(destDir.toFile, artifactsFilter)
          log.info(s"Found artifacts: ${artifacts.map(_.getName).mkString(", ")}")

          val hashes = artifacts.map { file =>
            val xs = Using.fileInputStream(file)(mk("SHA-256", _))
            file.toPath.getFileName.toString -> Hex.encodeHexString(xs)
          }

          // takeWhile to sort network builds
          val sortedHashes = hashes
            .filter { case (fileName, _) => !fileName.contains("devnet") }
            .sortBy { case (fileName, _) => (fileName.takeWhile(x => !x.isDigit).length, fileName) }

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
        createDirectories := Files.createDirectories((Compile / releaseDirectory).value),
        cleanAll := List.empty
      )) ++ Seq(
      commands += Command.command("packageAllForNetworks") { state =>
        NodeNetwork.All.foreach { x =>
          val updatedNetworkState = Project
            .extract(state)
            .appendWithoutSession(Seq(Global / network := x), state)

          Project.extract(updatedNetworkState).runTask(Compile / packageAll, updatedNetworkState)
        }

        state
      }
    )
}

trait ReleasePluginKeys {
  val releaseDirectory   = settingKey[Path]("Directory for release artifacts")
  val artifactExtensions = settingKey[Seq[String]]("Used to search artifacts")

  val release           = taskKey[Unit]("Packages artifacts and writes a documentation to the 'releaseDirectory'")
  val cleanAll          = taskKey[Unit]("Cleans all projects")
  val packageAll        = taskKey[Unit]("Packages all artifacts and moves them to 'releaseDirectory'")
  val createDirectories = taskKey[Unit]("Creates required directories")
  val genDocs           = taskKey[Unit]("Generates the documentation")
  val writeReleaseNotes = taskKey[Unit](s"""1. Collects commits between last two tags
                                           |2. Writes a draft of a release to the 'releaseDirectory'
                                           |3. Appends checksum of artifacts to release nodes""".stripMargin)
}
