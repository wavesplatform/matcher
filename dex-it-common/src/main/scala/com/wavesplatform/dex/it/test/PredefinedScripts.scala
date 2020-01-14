package com.wavesplatform.dex.it.test

import com.wavesplatform.dex.domain.bytes.ByteStr

object PredefinedScripts {
  val alwaysTrue: ByteStr  = from("AgZ7TN8j")
  val alwaysFalse: ByteStr = from("AgeJ1sz7")

  private def from(x: String): ByteStr = ByteStr.decodeBase64(x).get
}
