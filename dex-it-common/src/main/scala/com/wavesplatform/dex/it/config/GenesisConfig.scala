package com.wavesplatform.dex.it.config

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.AddressScheme

object GenesisConfig {
  val generatorConfig: Config = ConfigFactory.parseResources("genesis.conf")
  val config: Config = GenesisConfigGenerator.generate(generatorConfig)

  val chainId = config.getString("waves.blockchain.custom.address-scheme-character").head.toByte

  def setupAddressScheme(): Unit =
    if (AddressScheme.current.chainId != chainId)
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = GenesisConfig.chainId
      }

  setupAddressScheme()
}
