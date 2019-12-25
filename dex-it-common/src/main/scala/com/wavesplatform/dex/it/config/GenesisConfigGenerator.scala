package com.wavesplatform.dex.it.config

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.settings.{GenesisSettings, _}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

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

  case class FullAddressInfo(seedText: SeedText,
                             seed: ByteStr,
                             accountSeed: ByteStr,
                             accountPrivateKey: ByteStr,
                             accountPublicKey: ByteStr,
                             accountAddress: Address)

  private def toFullAddressInfo(item: DistributionItem): FullAddressInfo = {

    val seedBytes = item.seedText.getBytes("UTF-8")
    val acc       = Wallet.generateNewAccount(seedBytes, item.nonce)

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

    val generatorSettings = genesisGeneratorConfig.as[Settings]("genesis-generator")

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

      val reference     = ByteStr(Array.fill(SignatureLength)(-1: Byte))
      val genesisSigner = KeyPair(ByteStr.empty)

      Block
        .buildAndSign(
          version = 1,
          timestamp = timestamp,
          reference = reference,
          consensusData = NxtLikeConsensusBlockData(generatorSettings.baseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte))),
          transactionData = genesisTxs,
          signer = genesisSigner,
          featureVotes = Set.empty,
          rewardVote = 100L
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
         |    signature: "${settings.signature.get}"
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
