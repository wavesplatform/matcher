package com.wavesplatform.dex.load

import com.typesafe.config.Config
import com.wavesplatform.wavesj.{Node, PrivateKeyAccount}

class Environment(conf: Config) {
  val networkByte      = conf.getString("networkByte").charAt(0).toByte
  val node             = new Node(conf.getString("node"), networkByte)
  val matcherPublicKey = conf.getString("matcherPublicKey")
  val issuer           = PrivateKeyAccount.fromSeed(conf.getString("bank"), 0, networkByte)

  val assetQuantity = conf.getLong("assets.quantity")
  val issueFee      = conf.getLong("assets.issueFee")

  val loadHost = conf.getString("loadhost")
  val apiKey = conf.getString("api-key")
}
