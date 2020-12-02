import java.io.{File => _, FileFilter => _}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import gigahorse.Request
import org.apache.commons.io.FileUtils
import sbt.Keys.{streams, unmanagedBase}
import sbt._
import sbt.internal.util.ManagedLogger
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
    isOldWavesVersion := {
      val versionFile = unmanagedBase.value / "version"
      !(versionFile.isFile && IO.read(versionFile, StandardCharsets.UTF_8).trim() == wavesNodeVersion.value)
    },
    cleanupWavesNodeArtifacts := {
      if (isOldWavesVersion.value) {
        val filesToRemove = IO.listFiles(
          unmanagedBase.value,
          new FileFilter {
            override def accept(pathname: File): Boolean = pathname.getName != ".gitignore"
          }
        )

        filesToRemove.foreach { x =>
          if (x.isDirectory) FileUtils.deleteDirectory(x)
          else x.delete()
        }

        filesToRemove
      } else List.empty
    },
    downloadWavesNodeArtifacts := {
      implicit val log = streams.value.log

      if (isOldWavesVersion.value) {
        val version = wavesNodeVersion.value
        val targetDir = unmanagedBase.value

        val cacheDir = wavesArtifactsCacheDir.value
        val (preCachedXs, toCacheXs) = cacheInfo(artifactNames(version), cacheDir)
        val cachedXs =
          if (toCacheXs.isEmpty) {
            log.info("All required artifacts have been downloaded")
            preCachedXs
          } else {
            log.info("Opening the releases page...")
            val request = Request("https://api.github.com/repos/wavesplatform/Waves/releases").withHeaders("User-Agent" -> "SBT")
            val cached = Http.http.run(request).map { releasesContent =>
              log.info(s"Looking for $version version...")
              getFilesDownloadUrls(releasesContent.bodyAsString, version, toCacheXs).map { rawUrl =>
                val url = new URL(rawUrl)
                val fileName = url.getPath.split('/').last
                val cachedFile = cacheDir / fileName

                log.info(s"Downloading $url to $cachedFile...")
                Using.urlInputStream(url)(IO.transfer(_, cachedFile))

                cachedFile
              }
            }
            val cachedXs = Await.result(cached, 10.minutes)
            preCachedXs ::: cachedXs
          }

        val toTargetXs = toTarget(cachedXs, targetDir)
        if (toTargetXs.isEmpty) log.info("All required artifacts copied")
        else toTargetXs.foreach { x =>
          val targetFile = targetDir / x.getName
          log.info(s"Copying $x to $targetFile")
          IO.copyFile(x, targetFile)
        }

        // Additional steps
        // Unpacking grpc-server extension JARs to have all required dependencies, including grpc ones
        val grpcServerArchive = targetDir.listFiles(new FileFilter {
          override def accept(pathname: File): Boolean = pathname.name.matches("grpc-server.+\\.tgz")
        }).headOption.getOrElse(throw new RuntimeException("Can'f find a grpc-server archive"))

        MatcherIOUtils.decompressTgz(grpcServerArchive, targetDir)
        val grpcServerDir = targetDir / grpcServerArchive.name.replace(".tgz", "")
        IO.move(
          (grpcServerDir / "lib").listFiles().map { orig =>
            orig -> targetDir / orig.name
          }
        )
        FileUtils.deleteDirectory(grpcServerDir)

        // Write version file to prevent rewrites on each compilation
        val versionFile = unmanagedBase.value / "version"
        IO.write(versionFile, version, StandardCharsets.UTF_8, append = false)
      }
    },
    downloadWavesNodeArtifacts := downloadWavesNodeArtifacts.dependsOn(cleanupWavesNodeArtifacts).value
  )

  private val networkSuffixes = List("", "-stagenet")

  // List[Alternatives]
  private def artifactNames(version: String): List[List[String]] = List(
    List(s"waves-all-$version.jar"),
    networkSuffixes.map(x => s"waves_$x${version}_all.deb"),
    networkSuffixes.map(x => s"grpc-server$x-$version.tgz"),
    networkSuffixes.map(x => s"grpc-server${x}_${version}_all.deb")
  )

  private def getFilesDownloadUrls(rawJson: String, version: String, fileNamesToDownload: List[List[String]])(implicit
    log: ManagedLogger
  ): List[String] =
    Parser.parseFromString(rawJson).get match {
      case JArray(jReleases) =>
        jReleases
          .collectFirst {
            case JObject(jRelease) if jRelease.contains(JField("tag_name", JString(s"v$version"))) => jRelease
          }
          .map { jRelease =>
            jRelease.find(_.field == "assets") match {
              case Some(JField(_, JArray(jAssets))) => jAssets
              case x => throw new RuntimeException(s"Can't find assets in: $x")
            }
          }
          .map { jAssets =>
            fileNamesToDownload.flatMap { xs =>
              val r = xs.flatMap(findAssetUrl(jAssets, _))
              if (r.isEmpty) log.err(s"At least one artifact should be released: ${xs.mkString(", ")}")
              r.headOption
            }
          }
          .getOrElse {
            log.warn(s"Can't find version: $version (tag_name=v$version)")
            List.empty
          }
      case x => throw new RuntimeException(s"Can't parse releases as array: $x")
    }

  private def findAssetUrl(jAssets: Array[JValue], name: String)(implicit log: ManagedLogger): Option[String] =
    jAssets.collectFirst {
      case JObject(jAsset) if jAsset.contains(JField("name", JString(name))) =>
        jAsset
          .find(_.field == "browser_download_url")
          .getOrElse(throw new RuntimeException(s"Can't find browser_download_url in ${jAsset.mkString("Array(", ", ", ")")}"))
          .value match {
          case JString(x) => x
          case x => throw new RuntimeException(s"Can't parse url: $x")
        }
    }

  /**
   * @return (cached, toCache)
   */
  private def cacheInfo(alternatives: List[List[String]], cacheDir: File): (List[File], List[List[String]]) = {
    val (cachedArtifacts, toCache) = alternatives.partition(xs => xs.exists(x => (cacheDir / x).isFile))
    val cached = cachedArtifacts.map { xs =>
      xs.map(cacheDir / _).find(_.isFile).getOrElse(throw new RuntimeException("Imposibru!"))
    }
    (cached, toCache)
  }

  private def toTarget(cached: List[File], targetDir: File): List[File] =
    cached.filterNot(x => (targetDir / x.getName).isFile)

}

trait WavesNodeArtifactsKeys {
  // Useful for CI
  val wavesArtifactsCacheDir = settingKey[File]("Where cached artifacts are stored")
  val wavesNodeVersion = settingKey[String]("Waves Node version without 'v'")
  val cleanupWavesNodeArtifacts = taskKey[Seq[File]]("Removes stale artifacts")
  val downloadWavesNodeArtifacts = taskKey[Unit]("Downloads Waves Node artifacts to unmanagedBase")
  val isOldWavesVersion = taskKey[Boolean]("Returns true if there are outdated artifacts")
}
