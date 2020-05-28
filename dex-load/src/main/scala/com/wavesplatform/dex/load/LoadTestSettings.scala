package com.wavesplatform.dex.load

import com.typesafe.config.Config
import com.wavesplatform.wavesj.{Node, PrivateKeyAccount}

final class LoadTestSettings(conf: Config) {
  val root = "waves.dex.load"

  val networkByte      = conf.getString(s"$root.network-byte").charAt(0).toByte
  val matcherPublicKey = conf.getString(s"$root.matcher-public-key")
  val matcherUrl = conf.getString(s"$root.hosts.matcher")
  val issuer           = PrivateKeyAccount.fromSeed(conf.getString(s"$root.rich-account"), 0, networkByte)

  val node     = new Node(conf.getString(s"$root.hosts.node"), networkByte)
  val matcher  = new Node(conf.getString(s"$root.hosts.matcher"), networkByte)

  val loadHost = conf.getString(s"$root.hosts.shooted")
  val apiKey   = conf.getString(s"$root.dex-rest-api-key")

  val assetQuantity = conf.getLong(s"$root.assets.quantity")
  val issueFee      = conf.getLong(s"$root.assets.issue-fee")
}
