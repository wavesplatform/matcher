package com.wavesplatform.dex.grpc.integration.sync

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.grpc.integration.config.Accounts._
import com.wavesplatform.dex.grpc.integration.config.Assets._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.{DEXClient, ItTestSuiteBase}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV2, Order, OrderType}

import scala.concurrent.duration.DurationInt
import scala.util.Random

class WavesGrpcSyncClientTestSuite extends ItTestSuiteBase {

  private val target               = s"${node.networkAddress.getHostString}:${nodes.head.nodeExternalPort(6887)}"
  private lazy val wavesSyncClient = new DEXClient(target).wavesBlockchainSyncClient

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    node.signedBroadcast(IssueEthTx.json())
    node.waitForTransaction(IssueEthTx.id().toString)
  }

  "wasForged" - {
    val tx = IssueBtcTx
    val id = tx.id()

    "false for unknown tx" in {
      wavesSyncClient.wasForged(id) shouldBe false
    }

    "true for forged tx" in {
      node.signedBroadcast(tx.json())
      node.waitForTransaction(id.toString)
      wavesSyncClient.wasForged(id) shouldBe true
    }
  }

  "broadcastTx" - {
    "returns true if the transaction passed the validation and was added to the UTX pool" in {
      val pair       = AssetPair.createAssetPair(IssueEthTx.id().toString, "WAVES").get // TODO
      val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcher = matcher)

      wavesSyncClient.broadcastTx(exchangeTx) shouldBe true
      node.waitForTransaction(exchangeTx.id().toString)
    }

    "returns false if the transaction didn't pass the validation" in {
      val now = System.currentTimeMillis()

      val executedAmount = 1L
      val executedPrice  = 2 * Order.PriceConstant
      val pair           = AssetPair.createAssetPair(IssueEthTx.id().toString, "WAVES").get
      val buy            = mkOrder(alice, pair, OrderType.BUY, executedAmount, executedPrice, matcher = matcher)
      val sell           = mkOrder(KeyPair("fake-bob".getBytes(StandardCharsets.UTF_8)), pair, OrderType.SELL, executedAmount, executedPrice, matcher = matcher)

      val exchangeTx = ExchangeTransactionV2
        .create(
          matcher = matcher,
          buyOrder = buy,
          sellOrder = sell,
          amount = executedAmount,
          price = executedPrice,
          buyMatcherFee = matcherFee,
          sellMatcherFee = matcherFee,
          fee = matcherFee,
          timestamp = now
        )
        .explicitGet()

      wavesSyncClient.broadcastTx(exchangeTx) shouldBe false
    }
  }

  "isFeatureActivated" - {
    "returns false for not yet activated feature" in {
      wavesSyncClient.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance.id) shouldBe false
    }

    "returns true for activated feature" in {
      wavesSyncClient.isFeatureActivated(BlockchainFeatures.NG.id) shouldBe true
    }
  }

  "assetDescription" - {
    "returns None if there is no such asset" in {
      wavesSyncClient.assetDescription(IssuedAsset(IssueUsdTx.id())) shouldBe None
    }

    "returns an information for created assets" in {
      val issueTx = IssueEthTx
      wavesSyncClient.assetDescription(IssuedAsset(issueTx.id())) shouldBe Some(
        BriefAssetDescription(
          name = ByteStr(issueTx.name),
          decimals = issueTx.decimals,
          hasScript = issueTx.script.nonEmpty
        ))
    }
  }

  "hasScript/runScript(IssuedAsset)" - {
    "hasn't a script" in {
      wavesSyncClient.hasScript(IssuedAsset(IssueEthTx.id())) shouldBe false
    }

    "has a script" in {
      val issueTx = mkIssue(bob, "SmartCoin", defaultAssetQuantity, 8, smartIssueFee, Some(ExprScript(Terms.TRUE).explicitGet()))

      withClue("issue scripted asset") {
        node.broadcastRequest(issueTx.json())
        node.waitForTransaction(issueTx.id().toString)

        wavesSyncClient.hasScript(IssuedAsset(issueTx.id())) shouldBe true
      }

      withClue("run script") {
        val pair       = AssetPair.createAssetPair(issueTx.id().toString, "WAVES").get
        val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcherFee = 1.waves, matcher = matcher)

        wavesSyncClient.runScript(IssuedAsset(issueTx.id()), exchangeTx) shouldBe RunScriptResult.Allowed
      }
    }
  }

  "hasScript/runScript(Address)" - {
    "returns false if there is no script" in {
      wavesSyncClient.hasScript(matcher) shouldBe false
    }

    "returns true if a script was assigned" in {
      val receiver = KeyPair("receiver".getBytes(StandardCharsets.UTF_8))

      withClue("transfer") {
        val transferTx = mkTransfer(alice, receiver, 5.waves, Waves)
        node.broadcastRequest(transferTx.json())
        node.waitForTransaction(transferTx.id().toString)
      }

      withClue("set script") {
        val setScriptTx = mkSetAccountScript(receiver, script = Some(ExprScript(Terms.TRUE).explicitGet()))
        node.broadcastRequest(setScriptTx.json())
        node.waitForTransaction(setScriptTx.id().toString)

        wavesSyncClient.hasScript(receiver) shouldBe true
      }

      withClue("run script") {
        val now  = System.currentTimeMillis()
        val pair = AssetPair.createAssetPair(IssueEthTx.id().toString, "WAVES").get
        val buy  = Order.buy(bob, matcher, pair, 1L, 2 * Order.PriceConstant, now, now + 1.day.toMillis, 0)

        wavesSyncClient.runScript(receiver, buy) shouldBe RunScriptResult.Allowed
      }
    }
  }

  "spendableBalance" in {
    wavesSyncClient.spendableBalance(bob, Waves) shouldBe 494994799299998L
    wavesSyncClient.spendableBalance(bob, randomIssuedAsset) shouldBe 0L
  }

  "forgedOrder" - {
    "no such order" in {
      wavesSyncClient.forgedOrder(randomByteStr(32)) shouldBe false
    }

    "the order was in a forged ExchangeTransaction" in {
      val pair       = AssetPair.createAssetPair(IssueEthTx.id().toString, "WAVES").get
      val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcher = matcher)

      node.broadcastRequest(exchangeTx.json())
      node.waitForTransaction(exchangeTx.id().toString)

      wavesSyncClient.forgedOrder(exchangeTx.buyOrder.id()) shouldBe true
      wavesSyncClient.forgedOrder(exchangeTx.sellOrder.id()) shouldBe true
    }
  }

  // TODO check that the functions returns new data after the state is changed?

  private def randomByteStr(len: Int): ByteStr = {
    val inner = new Array[Byte](len)
    Random.nextBytes(inner)
    ByteStr(inner)
  }

  private def randomIssuedAsset: IssuedAsset = IssuedAsset(randomByteStr(32))
}
