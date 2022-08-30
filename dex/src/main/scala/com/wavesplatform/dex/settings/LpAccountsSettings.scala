package com.wavesplatform.dex.settings

import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.utils._

case class LpAccountsSettings(seed: AccountStorage.Settings, num: Int, custom: Set[PublicKey]) {

  // TODO generate lp accounts
  val accounts: Set[PublicKey] = custom

}
