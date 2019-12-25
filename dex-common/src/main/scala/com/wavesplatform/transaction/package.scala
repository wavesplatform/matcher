package com.wavesplatform

import com.wavesplatform.utils.base58Length

package object transaction {

  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

}
