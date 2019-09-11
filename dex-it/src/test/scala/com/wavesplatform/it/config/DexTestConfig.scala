package com.wavesplatform.it.config

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import com.typesafe.config.ConfigFactory.parseString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.market.MatcherActor
import com.wavesplatform.dex.AssetPairBuilder
import com.wavesplatform.it.api.FeeConstants._
import com.wavesplatform.it.dex.DoubleOps._
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.settings._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.collection.JavaConverters._
import scala.util.Random

object DexTestConfig {

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'Y'.toByte
  }

  private val containerConfigCache = new AtomicReference[Map[String, Config]](Map.empty)

  def containerConfig(name: String): Config = {
    containerConfigCache
      .updateAndGet { prev: Map[String, Config] =>
        if (prev isDefinedAt name) prev
        else
          prev.updated(
            name, {
              val baseConfig = if (name.startsWith("waves-")) genesisConfig else ConfigFactory.empty()
              baseConfig.withFallback(ConfigFactory.parseResources(s"nodes/$name.conf"))
            }
          )
      }
      .apply(name)
  }

  val genesisConfig: Config = genesisOverride

  val accounts: Map[String, KeyPair] = {

    val config           = ConfigFactory.parseResources("genesis.conf")
    val distributionsKey = "genesis-generator.distributions"
    val distributions    = config.getObject(distributionsKey)

    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix   = s"$distributionsKey.$accountName"
        val seedText = config.getString(s"$prefix.seed-text")
        val nonce    = config.getInt(s"$prefix.nonce")
        accountName -> Wallet.generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

  val matcher: KeyPair = accounts("matcher")
  val alice: KeyPair   = accounts("alice")
  val bob: KeyPair     = accounts("bob")

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"
  val btcAssetName = "BTC-X"

  val defaultAssetQuantity = 999999999999L

  val IssueUsdTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueEthTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = ethAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueBtcTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = btcAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val BtcId: ByteStr = IssueBtcTx.id()
  val EthId: ByteStr = IssueEthTx.id()
  val UsdId: ByteStr = IssueUsdTx.id()
  val WctId: ByteStr = IssueWctTx.id()

  val btc = IssuedAsset(BtcId)
  val eth = IssuedAsset(EthId)
  val usd = IssuedAsset(UsdId)
  val wct = IssuedAsset(WctId)

  val BtcAsset = IssuedAsset(BtcId)
  val EthAsset = IssuedAsset(EthId)
  val UsdAsset = IssuedAsset(UsdId)
  val WctAsset = IssuedAsset(WctId)

  val wctUsdPair = AssetPair(
    amountAsset = wct,
    priceAsset = usd
  )

  val wctWavesPair = AssetPair(
    amountAsset = wct,
    priceAsset = Waves
  )

  val ethWavesPair = AssetPair(
    amountAsset = eth,
    priceAsset = Waves
  )

  val ethBtcPair = AssetPair(
    amountAsset = eth,
    priceAsset = btc
  )

  val wavesUsdPair = AssetPair(
    amountAsset = Waves,
    priceAsset = usd
  )

  val ethUsdPair = AssetPair(
    amountAsset = eth,
    priceAsset = usd
  )

  val wavesBtcPair = AssetPair(
    amountAsset = Waves,
    priceAsset = btc
  )

  val orderLimit = 10

  val ForbiddenAssetId = ByteStr.decodeBase58("FdbnAsset").get // Hardcoded in dex-1.conf
  val ForbiddenAsset   = IssuedAsset(ForbiddenAssetId)

  val updatedMatcherConfig = parseString(s"""waves.dex {
                                            |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
                                            |  rest-order-limit = $orderLimit
                                            |}""".stripMargin)

  val NodesConfig: Config = ConfigFactory.parseResources("nodes.conf")

  private def genesisOverride: Config = {
    val genesisTs = System.currentTimeMillis()

    val timestampOverrides = parseString(s"""waves.blockchain.custom.genesis {
                                            |  timestamp = $genesisTs
                                            |  block-timestamp = $genesisTs
                                            |  signature = null # To calculate it in Block.genesis
                                            |}""".stripMargin)

    // TODO waves-base.conf
    val genesisConfig = timestampOverrides.withFallback(ConfigFactory.parseResources("nodes/waves-1.conf"))
    val gs            = genesisConfig.as[GenesisSettings]("waves.blockchain.custom.genesis")

//    AddressScheme.current = new AddressScheme {
//      override val chainId: Byte = genesisConfig.getString("waves.blockchain.custom.address-scheme-character").head.toByte
//    }

    val genesisSignature = Block.genesis(gs).explicitGet().uniqueId

    parseString(s"waves.blockchain.custom.genesis.signature = $genesisSignature").withFallback(timestampOverrides)
  }

  val Configs: Seq[Config] = Seq(
    updatedMatcherConfig.withFallback(NodesConfig.getConfigList("nodes").asScala.head)
  )

  def createAssetPair(asset1: String, asset2: String): AssetPair = {
    val (a1, a2) = (AssetPair.extractAssetId(asset1).get, AssetPair.extractAssetId(asset2).get)
    if (AssetPairBuilder.assetIdOrdering.compare(a1.compatId, a2.compatId) > 0)
      AssetPair(a1, a2)
    else
      AssetPair(a2, a1)
  }

  def createAssetPair(asset1: Asset, asset2: Asset): AssetPair =
    if (AssetPairBuilder.assetIdOrdering.compare(asset1.compatId, asset2.compatId) > 0) AssetPair(asset1, asset2)
    else AssetPair(asset2, asset1)

  def issueAssetPair(issuer: KeyPair, amountAssetDecimals: Byte, priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)
  }

  def issueAssetPair(amountAssetIssuer: KeyPair,
                     priceAssetIssuer: KeyPair,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {
    val issueAmountAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = amountAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = amountAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = priceAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), Some(issueAmountAssetTx.id().arr)) < 0) {
      (issueAmountAssetTx,
       issuePriceAssetTx,
       AssetPair(
         amountAsset = IssuedAsset(issueAmountAssetTx.id()),
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  def assetPairIssuePriceAsset(issuer: KeyPair, amountAssetId: Asset, priceAssetDecimals: Byte): (IssueTransaction, AssetPair) = {
    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = issuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), amountAssetId.compatId.map(_.arr)) < 0) {
      (issuePriceAssetTx,
       AssetPair(
         amountAsset = amountAssetId,
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      assetPairIssuePriceAsset(issuer, amountAssetId, priceAssetDecimals)
  }

}
