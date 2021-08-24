package com.wavesplatform.dex.db

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.dex.db.AccountStorage.Settings.EncryptedFile
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.settings.utils.{ConfigReaders, WrappedDescendantHint}
import pureconfig.generic.semiauto

import java.io.File

case class AccountStorage(keyPair: KeyPair)

object AccountStorage {

  sealed trait Settings

  object Settings {

    case class InMem(seedInBase64: ByteStr) extends Settings

    object InMem extends ConfigReaders {
      val empty: InMem = new InMem(ByteStr.empty)

      implicit val byteStrConfigReader = byteStr64ConfigReader
      implicit val inMemConfigReader = semiauto.deriveReader[InMem]
    }

    case class EncryptedFile(path: File, password: String) extends Settings

    object EncryptedFile {
      val empty: EncryptedFile = new EncryptedFile(new File(""), "")
    }

    implicit val accountStorageHint = new WrappedDescendantHint[Settings]()
  }

  def load(settings: Settings): Either[String, AccountStorage] = settings match {
    case Settings.InMem(seed) => Right(AccountStorage(KeyPair(seed)))
    case Settings.EncryptedFile(file, password) => SecuredFileStorage(file.getAbsolutePath, password).load()
  }

  def save(seed: ByteStr, to: EncryptedFile): Unit = SecuredFileStorage(to.path.getAbsolutePath, to.password).save(seed)

  def getAccountSeed(baseSeed: ByteStr, nonce: Int): ByteStr = ByteStr(crypto.secureHash(Bytes.concat(Ints.toByteArray(nonce), baseSeed)))
}
