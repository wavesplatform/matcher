import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils
import sbt._

object MatcherIOUtils {

  def decompressTgz(tarFile: File, dest: File): Unit = {
    val tarIn = new TarArchiveInputStream(
      new GzipCompressorInputStream(
        new BufferedInputStream(
          new FileInputStream(tarFile)
        )
      )
    )

    try {
      dest.mkdirs()
      Iterator
        .continually(tarIn.getNextEntry)
        .takeWhile(_ != null)
        .foreach { entry =>
          val name = entry.getName
          val path = dest / name

          if (entry.isDirectory) path.mkdirs()
          else if (!path.isFile) IOUtils.copy(tarIn, new FileOutputStream(path)) // isFile to check existence
        }
    } finally tarIn.close()
  }

}
