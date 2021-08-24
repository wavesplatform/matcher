package com.wavesplatform.dex.db

import cats.implicits.catsSyntaxEitherId
import com.wavesplatform.dex.crypto.Enigma
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.ByteStr

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer

final case class SecuredFileStorage(path: String, password: String) {

  val file = new File(path)

  private def readFile(file: File): Array[Byte] = {
    val reader = new FileInputStream(file)
    try {
      val buff = new Array[Byte](1024)
      val r = new ArrayBuffer[Byte]
      while (reader.available() > 0) {
        val read = reader.read(buff)
        if (read > 0)
          r.appendAll(buff.iterator.take(read))
      }
      r.toArray
    } finally reader.close()
  }

  private def writeFile(file: File, bytes: Array[Byte]): Unit = {
    val writer = new FileOutputStream(file, false)
    try writer.write(bytes)
    finally writer.close()
  }

  def save(seed: ByteStr): Unit = {
    Files.createDirectories(file.getParentFile.toPath)
    val key = Enigma.prepareDefaultKey(password)
    val encryptedSeedBytes = Enigma.encrypt(key, seed.arr)
    writeFile(file, encryptedSeedBytes)
  }

  def load(): Either[String, AccountStorage] =
    if (file.isFile) {
      val encryptedSeedBytes = readFile(file)
      val key = Enigma.prepareDefaultKey(password)
      val decryptedBytes = Enigma.decrypt(key, encryptedSeedBytes)
      AccountStorage(KeyPair(decryptedBytes)).asRight
    } else s"A file '${file.getAbsolutePath}' doesn't exist".asLeft

}
