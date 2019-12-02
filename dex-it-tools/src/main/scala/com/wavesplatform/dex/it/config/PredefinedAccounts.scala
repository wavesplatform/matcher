package com.wavesplatform.dex.it.config

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.config.GenesisConfig.generatorConfig
import com.wavesplatform.wallet.Wallet

import scala.collection.JavaConverters._

object PredefinedAccounts extends PredefinedAccounts

trait PredefinedAccounts {

  private val accounts: Map[String, KeyPair] = {

    val distributionsKey = "genesis-generator.distributions"
    val distributions    = generatorConfig.getObject(distributionsKey)

    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix   = s"$distributionsKey.$accountName"
        val seedText = generatorConfig.getString(s"$prefix.seed-text")
        val nonce    = generatorConfig.getInt(s"$prefix.nonce")
        accountName -> Wallet.generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

  val matcher: KeyPair = accounts("matcher")
  val alice: KeyPair   = accounts("alice")
  val bob: KeyPair     = accounts("bob")
}
