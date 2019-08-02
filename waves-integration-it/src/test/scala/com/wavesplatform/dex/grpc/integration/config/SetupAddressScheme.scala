package com.wavesplatform.dex.grpc.integration.config

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme

object SetupAddressScheme {
  // Right initialization
  private val genesisConfig = ConfigFactory.parseResources("genesis.conf")
  private val networkByte   = genesisConfig.getString("genesis-generator.network-type").head.toByte

  def setup(): Unit = if (AddressScheme.current.chainId != networkByte) {
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = networkByte
    }

    println(s"Providing network: ${AddressScheme.current.chainId}")
  }
}
