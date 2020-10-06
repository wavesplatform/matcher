import java.io.File
import java.nio.file.Paths

import sbt.Keys.streams
import sbt._
import sbt.io.Using

import scala.sys.process._

/** Downloads and caches artifacts required for IT images */
object ItCachedArtifactsPlugin extends AutoPlugin {

  object autoImport extends ItCachedArtifactsKeys
  import autoImport._

  override val trigger = PluginTrigger.NoTrigger

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    itArtifactsCacheDir := Paths.get(System.getProperty("user.home"), ".cache", "dex").toFile,
    downloadItArtifacts := {

      val log = streams.value.log
      val cacheDir = itArtifactsCacheDir.value; cacheDir.mkdirs()

      log.info(s"Count of artifacts required for IT: ${itArtifactDescriptions.value.size}. Checking artifacts cache...")

      itArtifactDescriptions.value.zipWithIndex.foreach {
        case (ArtifactDescription(downloadUrl, maybeArtifactName, maybeScript), idx) =>
          val artifactIdx = s"[${idx + 1}]"
          val artifactUrl = new URL(downloadUrl)
          val artifactName = maybeArtifactName getOrElse artifactUrl.getPath.split('/').last
          val cachedFile = cacheDir / artifactName

          if (cachedFile.exists) log.info(s"$artifactIdx Artifact $artifactName found in the cache")
          else {
            log.info(s"$artifactIdx Downloading $artifactUrl to $cachedFile...")
            Using.urlInputStream(artifactUrl)(IO.transfer(_, cachedFile))
            maybeScript foreach { script =>
              log.info(s"Executing additional script for $artifactName, result = ${script.!}")
            }
          }
      }

      log.info(s"IT artifacts have been prepared")
    }
  )

}

trait ItCachedArtifactsKeys {

  case class ArtifactDescription(downloadUrl: String, name: Option[String] = None, additionalScript: Option[String] = None)

  val itArtifactsCacheDir = settingKey[File]("Where cached artifacts are stored")
  val itArtifactDescriptions = settingKey[Seq[ArtifactDescription]]("Artifacts descriptions")
  val downloadItArtifacts = taskKey[Unit]("Downloads artifacts required for IT to the cache directory")
}
