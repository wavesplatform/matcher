import java.io.InputStream
import java.nio.charset.StandardCharsets

import org.apache.commons.codec.binary.Hex
import sbt.{File, IO}
import sbt.io.Using

object ReleaseHelpers {
  def writeHashesFile(dest: File, artifacts: Seq[File]): Unit = {
    val hashes = artifacts.map { file =>
      val xs = Using.fileInputStream(file)(hash("SHA-256", _))
      file.toPath.getFileName -> Hex.encodeHexString(xs)
    }

    val hashesFileContent =
      s"""## SHA256 Checksums
         |
       |```
         |${hashes.map { case (fileName, hash) => s"$hash $fileName" }.mkString("\n")}
         |```
         |""".stripMargin

    IO.write(dest, hashesFileContent, StandardCharsets.UTF_8)
  }

  def hash(algorithm: String, stream: InputStream): Array[Byte] = {
    import java.security.{DigestInputStream, MessageDigest}
    val digest = MessageDigest.getInstance(algorithm)
    try {
      val dis    = new DigestInputStream(stream, digest)
      val buffer = new Array[Byte](8192)
      while (dis.read(buffer) >= 0) {}
      dis.close()
      digest.digest
    } finally { stream.close() }
  }
}
