package com.wavesplatform.dex.settings

import com.wavesplatform.crypto.base.Base58
import com.wavesplatform.dex.domain.account.{Address, PublicKey}

import java.io._
import scala.jdk.CollectionConverters._

case class LpAccountsSettings(filePath: String) {

  lazy val publicKeys: Set[PublicKey] = {
    val stream = new FileInputStream(filePath)
    try {
      val streamReader = new InputStreamReader(stream)
      val bufferedReader = new BufferedReader(streamReader)
      bufferedReader.lines
        .filter(_.nonEmpty)
        .map[PublicKey](line => PublicKey(Base58.decode(line)))
        .iterator
        .asScala
        .toSet
    } finally stream.close()
  }

  lazy val addresses: Set[Address] = publicKeys.map(_.toAddress)

}
