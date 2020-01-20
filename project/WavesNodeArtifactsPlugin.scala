import java.io.File
import java.nio.file.Paths

import gigahorse.Request
import sbt.Keys.{streams, unmanagedBase}
import sbt._
import sbt.io.Using
import sbt.librarymanagement.Http
import sjsonnew.shaded.scalajson.ast.unsafe._
import sjsonnew.support.scalajson.unsafe.Parser

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

// Probably, JAR artifact should be downloaded through custom resolver
object WavesNodeArtifactsPlugin extends AutoPlugin {

  object autoImport extends WavesNodeArtifactsKeys
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    wavesArtifactsCacheDir := Paths.get(System.getProperty("user.home"), ".cache", "dex").toFile,
    cleanupWavesNodeArtifacts := {
      val version = wavesNodeVersion.value
      val filesToRemove = IO.listFiles(unmanagedBase.value).filter { x =>
        val name = x.getName
        name.startsWith("waves") && !name.contains(version)
      }

      filesToRemove.foreach(_.delete())
      filesToRemove
    },
    downloadWavesNodeArtifacts := {
      val version   = wavesNodeVersion.value
      val targetDir = unmanagedBase.value
      val log       = streams.value.log

      val unmanagedJarsToDownload = artifactNames(version).filterNot(x => (targetDir / x).isFile)
      if (unmanagedJarsToDownload.isEmpty) log.info("Waves Node artifacts have been downloaded")
      else {
        val cacheDir = wavesArtifactsCacheDir.value
        cacheDir.mkdirs()

        val (cachedArtifacts, artifactsToDownload) = unmanagedJarsToDownload.partition(x => (cacheDir / x).isFile)
        cachedArtifacts.foreach { x =>
          IO.copyFile(cacheDir / x, targetDir / x)
        }

        if (artifactsToDownload.isEmpty) log.info("Waves Node artifacts have been cached")
        else {
          log.info("Opening releases page...")
          val r = Http.http.run(Request("https://api.github.com/repos/wavesplatform/Waves/releases")).map {
            releasesContent =>
              log.info(s"Looking for Waves Node $version...")
              getFilesToDownload(releasesContent.bodyAsString, version, _ => artifactsToDownload).map { rawUrl =>
                val url        = new URL(rawUrl)
                val fileName   = url.getPath.split('/').last
                val cachedFile = cacheDir / fileName
                val targetFile = targetDir / fileName

                log.info(s"Downloading $url to $cachedFile...")
                Using.urlInputStream(url)(IO.transfer(_, cachedFile))

                log.info(s"Copying $cachedFile to $targetFile")
                IO.copyFile(cachedFile, targetFile)

                targetFile
              }
          }
          Await.ready(r, 10.minutes)
        }
      }
    },
    downloadWavesNodeArtifacts := downloadWavesNodeArtifacts.dependsOn(cleanupWavesNodeArtifacts).value
  )

  private def artifactNames(version: String): List[String] = List(s"waves-all-$version.jar", s"waves_${version}_all.deb")

  /**
    * @param toDownload version => file names to download
    */
  private def getFilesToDownload(rawJson: String, version: String, toDownload: String => List[String]): List[String] =
    Parser.parseFromString(rawJson).get match {
      case JArray(jReleases) =>
        jReleases
          .collectFirst {
            case JObject(jRelease) if jRelease.contains(JField("tag_name", JString(s"v$version"))) =>
              jRelease.find(_.field == "assets") match {
                case Some(JField(_, JArray(jAssets))) => toDownload(version).map(findAssetUrl(jAssets, _))
                case x                                => throw new RuntimeException(s"Can't find assets in: $x")
              }
          }
          .getOrElse(throw new RuntimeException(s"Can't find version: $version (tag_name=v$version)"))
      case x => throw new RuntimeException(s"Can't parse releases as array: $x")
    }

  private def findAssetUrl(jAssets: Array[JValue], name: String): String = {
    jAssets
      .collectFirst {
        case JObject(jAsset) if jAsset.contains(JField("name", JString(name))) =>
          jAsset
            .find(_.field == "browser_download_url")
            .getOrElse(throw new RuntimeException(s"Can't find browser_download_url in $jAsset"))
            .value match {
            case JString(x) => x
            case x          => throw new RuntimeException(s"Can't parse url: $x")
          }
      }
      .getOrElse(throw new RuntimeException(s"Can't find $name"))
  }
}

trait WavesNodeArtifactsKeys {
  // Useful for CI
  val wavesArtifactsCacheDir     = settingKey[File]("Where cached artifacts are stored")
  val wavesNodeVersion           = settingKey[String]("Waves Node version without 'v'")
  val cleanupWavesNodeArtifacts  = taskKey[Seq[File]]("Removes stale artifacts")
  val downloadWavesNodeArtifacts = taskKey[Unit]("Downloads Waves Node artifacts to unmanagedBase")
}
