package com.wavesplatform.dex.db

import com.wavesplatform.account.KeyPair

case class AccountStorage(keyPair: KeyPair)
object AccountStorage {
  case class AccountStorageSettings()

  def load(settings: AccountStorageSettings): Either[String, AccountStorage] = ???
}
