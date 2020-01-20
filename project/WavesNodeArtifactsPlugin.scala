import java.io.File

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

object WavesNodeArtifactsPlugin extends AutoPlugin {

  object autoImport extends WavesNodeArtifactsKeys
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
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
      val version = wavesNodeVersion.value
      val destDir = unmanagedBase.value
      val log     = streams.value.log

      val artifactsToDownload = artifactNames(version).filterNot(x => (destDir / x).isFile)
      if (artifactsToDownload.isEmpty) {
        log.info("Waves Node artifacts have been downloaded")
        List.empty
      } else {
        log.info("Opening releases page...")
        val r = Http.http.run(Request("https://api.github.com/repos/wavesplatform/Waves/releases")).map { releasesContent =>
          log.info(s"Looking for Waves Node $version...")
          getFilesToDownload(releasesContent.bodyAsString, version, _ => artifactsToDownload).map { rawUrl =>
            val url        = new URL(rawUrl)
            val targetFile = destDir / url.getPath.split('/').last
            log.info(s"Downloading $url to $targetFile")
            Using.urlInputStream(url)(IO.transfer(_, targetFile))
            targetFile
          }
        }
        Await.result(r, 10.minutes)
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
  val wavesNodeVersion           = settingKey[String]("Waves Node version without 'v'")
  val cleanupWavesNodeArtifacts  = taskKey[Seq[File]]("Removes stale artifacts")
  val downloadWavesNodeArtifacts = taskKey[Seq[File]]("Downloads Waves Node artifacts to unmanagedBase")
}
