package com.wavesplatform.dex.settings

import com.wavesplatform.crypto.base.Base58
import com.wavesplatform.dex.domain.account.PublicKey

import java.io._
import scala.jdk.CollectionConverters._

case class LpAccountsSettings(filePath: String) {

  val accounts: Set[PublicKey] = {
    val file = new File(filePath)
    val stream = new FileInputStream(file)
    val streamReader = new InputStreamReader(stream)
    val bufferedReader = new BufferedReader(streamReader)
    bufferedReader.lines
      .filter(_.nonEmpty)
      .map[PublicKey](line => PublicKey(Base58.decode(line)))
      .iterator
      .asScala
      .toSet
  }

}
