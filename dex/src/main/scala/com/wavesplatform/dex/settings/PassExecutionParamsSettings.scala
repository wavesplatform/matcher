package com.wavesplatform.dex.settings

import com.wavesplatform.dex.domain.account.PublicKey

final case class PassExecutionParamsSettings(sinceOffset: Long, forAccounts: Set[PublicKey])
