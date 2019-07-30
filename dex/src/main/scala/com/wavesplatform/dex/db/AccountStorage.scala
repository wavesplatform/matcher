package com.wavesplatform.dex.db

import java.io.File
import java.nio.file.Files

import cats.syntax.either._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.dex.db.AccountStorage.Settings.EncryptedFile
import com.wavesplatform.utils.JsonFileStorage
import net.ceedubs.ficus.readers.ValueReader

import scala.util.Try

case class AccountStorage(keyPair: KeyPair)
object AccountStorage {
  sealed trait Settings
  object Settings {
    case class InMem(seed: ByteStr)                        extends Settings
    case class EncryptedFile(path: File, password: String) extends Settings

    implicit val valueReader: ValueReader[Settings] = ValueReader.relative[Settings] { config =>
      config.getString("type") match {
        case "in-mem" => InMem(Base64.decode(config.getString("in-mem.seed-in-base64")))
        case "encrypted-file" =>
          EncryptedFile(
            path = new File(config.getString("encrypted-file.path")),
            password = config.getString("encrypted-file.password")
          )
        case x => throw new IllegalArgumentException(s"The type of account storage '$x' is unknown. Please update your settings.")
      }
    }
  }

  def load(settings: Settings): Either[String, AccountStorage] = settings match {
    case Settings.InMem(seed) => Right(AccountStorage(KeyPair(seed)))
    case Settings.EncryptedFile(file, password) =>
      for {
        _ <- Either.cond(file.isFile, (), s"A file '${file.getAbsolutePath}' doesn't exist")
        encodedContent <- Try(JsonFileStorage.load[String](file.getAbsolutePath, Some(JsonFileStorage.prepareKey(password)))).toEither
          .leftMap(_.getMessage)
        decodedContent <- ByteStr.decodeBase64(encodedContent).toEither.leftMap(_.getMessage)
      } yield AccountStorage(KeyPair(decodedContent))
  }

  def save(seed: ByteStr, to: EncryptedFile): Unit = {
    Files.createDirectories(to.path.getParentFile.toPath)
    JsonFileStorage.save(seed.base64Raw, to.path.getAbsolutePath, Some(JsonFileStorage.prepareKey(to.password)))
  }
}
