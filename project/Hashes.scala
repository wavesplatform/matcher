import java.io.InputStream

object Hashes {

  def mk(algorithm: String, stream: InputStream): Array[Byte] = {
    import java.security.{DigestInputStream, MessageDigest}
    val digest = MessageDigest.getInstance(algorithm)
    try {
      val dis = new DigestInputStream(stream, digest)
      val buffer = new Array[Byte](8192)
      while (dis.read(buffer) >= 0) {}
      dis.close()
      digest.digest
    } finally stream.close()
  }

}
