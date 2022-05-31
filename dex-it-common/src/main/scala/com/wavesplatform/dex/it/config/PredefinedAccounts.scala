package com.wavesplatform.dex.it.config

import java.nio.charset.StandardCharsets
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.{Blake2b256, Keccak256}
import com.wavesplatform.dex.it.config.GenesisConfig.generatorConfig

import scala.jdk.CollectionConverters._

object PredefinedAccounts extends PredefinedAccounts {
  def secureHash(m: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(m))
  def generateNewAccount(seed: Array[Byte], nonce: Int): KeyPair = KeyPair(secureHash(Bytes.concat(Ints.toByteArray(nonce), seed)))
}

trait PredefinedAccounts {

  import PredefinedAccounts._

  private val accounts: Map[String, KeyPair] = {

    val distributionsKey = "genesis-generator.distributions"
    val distributions = generatorConfig.getObject(distributionsKey)

    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix = s"$distributionsKey.$accountName"
        val seedText = generatorConfig.getString(s"$prefix.seed-text")
        val nonce = generatorConfig.getInt(s"$prefix.nonce")
        accountName -> generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

  val matcher: KeyPair = accounts("matcher")
  val alice: KeyPair = accounts("alice")
  val bob: KeyPair = accounts("bob")
}
