package com.wavesplatform.dex.it.config

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.it.config.genesis._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration

object GenesisConfigGenerator {

  private type AccountName = String
  private type SeedText    = String
  private type Share       = Long

  case class DistributionItem(seedText: String, nonce: Int, amount: Share)

  case class Settings(networkType: String,
                      initialBalance: Share,
                      baseTarget: Long,
                      averageBlockDelay: FiniteDuration,
                      timestamp: Option[Long],
                      distributions: Map[AccountName, DistributionItem]) {

    private[this] val distributionsSum = distributions.values.map(_.amount).sum
    require(
      distributionsSum == initialBalance,
      s"The sum of all balances should be == $initialBalance, but it is $distributionsSum"
    )

    val chainId: Byte = networkType.head.toByte
  }

  object Settings {
    implicit val chosenCase: NameMapper                = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
    implicit val settingsReader: ValueReader[Settings] = arbitraryTypeValueReader[Settings]
  }

  case class FullAddressInfo(seedText: SeedText,
                             seed: ByteStr,
                             accountSeed: ByteStr,
                             accountPrivateKey: ByteStr,
                             accountPublicKey: ByteStr,
                             accountAddress: Address)

  private def toFullAddressInfo(item: DistributionItem): FullAddressInfo = {

    val seedBytes = item.seedText.getBytes("UTF-8")
    val acc       = PredefinedAccounts.generateNewAccount(seedBytes, item.nonce)

    FullAddressInfo(
      seedText = item.seedText,
      seed = ByteStr(seedBytes),
      accountSeed = ByteStr(acc.seed),
      accountPrivateKey = acc.privateKey,
      accountPublicKey = acc.publicKey,
      accountAddress = acc.toAddress
    )
  }

  def generate(genesisGeneratorConfig: Config): Config = {

    val generatorSettings = Settings.settingsReader.read(genesisGeneratorConfig, "genesis-generator")

    AddressScheme.current = new AddressScheme { override val chainId: Byte = generatorSettings.chainId }

    val shares: Seq[(AccountName, FullAddressInfo, Share)] =
      generatorSettings.distributions
        .map { case (accountName, distributionItem) => (accountName, toFullAddressInfo(distributionItem), distributionItem.amount) }
        .toSeq
        .sortBy(_._1)

    val timestamp = generatorSettings.timestamp.getOrElse(System.currentTimeMillis)

    val genesisTxs: Seq[GenesisTransaction] = shares.map {
      case (_, addrInfo, part) => GenesisTransaction(addrInfo.accountAddress, part, timestamp, ByteStr.empty)
    }

    val genesisBlock = {

      val reference     = ByteStr(Array.fill(crypto.SignatureLength)(-1: Byte))
      val genesisSigner = KeyPair(ByteStr.empty)

      Block
        .buildAndSign(
          version = 1,
          timestamp = timestamp,
          reference = reference,
          consensusData = NxtLikeConsensusBlockData(generatorSettings.baseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte))),
          transactionData = genesisTxs,
          signer = genesisSigner
        )
        .explicitGet()
    }

    val settings =
      GenesisSettings(
        genesisBlock.timestamp,
        timestamp,
        generatorSettings.initialBalance,
        Some(genesisBlock.signerData.signature),
        genesisTxs.map { tx =>
          GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)
        },
        generatorSettings.baseTarget,
        generatorSettings.averageBlockDelay
      )

    ConfigFactory.parseString(
      s"""waves.blockchain.custom {
         |  address-scheme-character = "${generatorSettings.chainId.toChar}"
         |  genesis {
         |    timestamp: ${settings.timestamp}
         |    signature: ${genesisBlock.signerData.signature}
         |    initial-balance: ${settings.initialBalance}
         |    initial-base-target: ${settings.initialBaseTarget}
         |    average-block-delay: ${settings.averageBlockDelay.toMillis}ms
         |    block-timestamp: ${settings.blockTimestamp}
         |    transactions = [
         |      ${settings.transactions.map(x => s"""{recipient: "${x.recipient}", amount: ${x.amount}}""").mkString(",\n    ")}
         |    ]
         |  }
         |}""".stripMargin
    )
  }
}
