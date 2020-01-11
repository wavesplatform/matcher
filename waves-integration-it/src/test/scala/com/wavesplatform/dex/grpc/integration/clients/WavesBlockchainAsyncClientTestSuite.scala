package com.wavesplatform.dex.grpc.integration.clients

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.{IntegrationSuiteBase, WavesBlockchainClientBuilder}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV2, Order, OrderType}
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import org.scalatest.Assertion

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

class WavesBlockchainAsyncClientTestSuite extends IntegrationSuiteBase {

  private val runNow = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  private implicit val monixScheduler = Scheduler(runNow)
  private lazy val client             = WavesBlockchainClientBuilder.async(wavesNode1.grpcApiTarget, 100.milliseconds, monixScheduler, runNow)

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(
    timeout = 1.minute,
    interval = 1.second
  )

  @volatile private var balanceChanges = Map.empty[Address, Map[Asset, Long]]

  private val eventsObserver: Observer[SpendableBalanceChanges] = new Observer[SpendableBalanceChanges] {
    override def onError(ex: Throwable): Unit                       = Unit
    override def onComplete(): Unit                                 = Unit
    override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balanceChanges ++= elem; Continue }
  }

  private def assertBalanceChanges(expectedBalanceChanges: Map[Address, Map[Asset, Long]]): Assertion = eventually {
    // Remove pairs (address, asset) those expectedBalanceChanges has not
    val actual = simplify(balanceChanges.filterKeys(expectedBalanceChanges.keys.toSet).map {
      case (address, balance) => address -> balance.filterKeys(expectedBalanceChanges(address).contains)
    })
    val expected = simplify(expectedBalanceChanges)
    actual should matchTo(expected)
  }

  private def simplify(xs: Map[Address, Map[Asset, Long]]): String =
    xs.toList
      .map {
        case (address, assets) =>
          val xs = assets
            .map { case (asset, v) => AssetPair.assetIdStr(asset) -> v }
            .toList
            .sortBy(_._1)
            .map { case (asset, v) => s"$v $asset" }
            .mkString(", ")
          address.stringRepr -> xs
      }
      .sortBy(_._1)
      .map {
        case (address, assets) => s"$address: ($assets)"
      }
      .mkString("; ")

  override def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
    client.spendableBalanceChanges.subscribe(eventsObserver)
  }

  "DEX client should receive balance changes via gRPC" in {
    val aliceInitialBalance = wavesNode1.api.balance(alice, Waves)

    val issueAssetTx = mkIssue(alice, "name", someAssetAmount, 2)
    val issuedAsset  = IssuedAsset(issueAssetTx.id.value)

    balanceChanges = Map.empty[Address, Map[Asset, Long]]
    broadcastAndAwait(issueAssetTx)

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee),
          issuedAsset -> someAssetAmount
        )
      )
    }

    balanceChanges = Map.empty[Address, Map[Asset, Long]]
    broadcastAndAwait(mkTransfer(alice, bob, someAssetAmount, issuedAsset))

    assertBalanceChanges {
      Map(
        alice.toAddress -> Map(
          Waves       -> (aliceInitialBalance - issueFee - minFee),
          issuedAsset -> 0L
        ),
        bob.toAddress -> Map(
          issuedAsset -> someAssetAmount
        )
      )
    }
  }

  "wasForged" - {
    "false for unknown tx" in {
      wait(client.wereForged(Seq(BtcId))).values.head shouldBe false
    }

    "true for forged tx" in {
      broadcastAndAwait(IssueBtcTx)
      wait(client.wereForged(Seq(BtcId))).values.head shouldBe true
    }
  }

  "broadcastTx" - {
    "returns true if the transaction passed the validation and was added to the UTX pool" in {
      val pair       = AssetPair.createAssetPair(UsdId.toString, "WAVES").get // TODO
      val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcher = matcher)

      wait(client.broadcastTx(exchangeTx)) shouldBe true
      wavesNode1.api.waitForTransaction(exchangeTx.id())
    }

    "returns false if the transaction didn't pass the validation" in {
      val now = System.currentTimeMillis()

      val executedAmount = 1L
      val executedPrice  = 2 * Order.PriceConstant
      val pair           = AssetPair.createAssetPair(UsdId.toString, "WAVES").get
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

      wait(client.broadcastTx(exchangeTx)) shouldBe false
    }
  }

  "isFeatureActivated" - {
    "returns false for not yet activated feature" in {
      wait(client.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)) shouldBe false
    }

    "returns true for activated feature" in {
      wait(client.isFeatureActivated(BlockchainFeatures.NG.id)) shouldBe true
    }
  }

  "assetDescription" - {
    "returns None if there is no such asset" in {
      wait(client.assetDescription(eth)) shouldBe None
    }

    "returns an information for created assets" in {
      val issueTx = IssueUsdTx
      wait(client.assetDescription(IssuedAsset(issueTx.id()))) should matchTo(
        Option(
          BriefAssetDescription(
            name = new String(issueTx.name, StandardCharsets.UTF_8),
            decimals = issueTx.decimals,
            hasScript = issueTx.script.nonEmpty
          )
        ))
    }
  }

  "hasScript/runScript(IssuedAsset)" - {
    "hasn't a script" in {
      wait(client.hasScript(usd)) shouldBe false
    }

    "has a script" in {
      val issueTx = mkIssue(bob, "SmartCoin", defaultAssetQuantity, 8, smartIssueFee, Some(ExprScript(Terms.TRUE).explicitGet()))

      withClue("issue scripted asset") {
        broadcastAndAwait(issueTx)

        wait(client.hasScript(IssuedAsset(issueTx.id()))) shouldBe true
      }

      withClue("run script") {
        val pair       = AssetPair.createAssetPair(issueTx.id().toString, "WAVES").get
        val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcherFee = 1.waves, matcher = matcher)

        wait(client.runScript(IssuedAsset(issueTx.id()), exchangeTx)) shouldBe RunScriptResult.Allowed
      }
    }
  }

  "hasScript/runScript(Address)" - {
    "returns false if there is no script" in {
      wait(client.hasScript(matcher)) shouldBe false
    }

    "returns true if a script was assigned" in {
      val receiver = KeyPair("receiver".getBytes(StandardCharsets.UTF_8))

      withClue("transfer") {
        broadcastAndAwait(mkTransfer(alice, receiver, 5.waves, Waves))
      }

      withClue("set script") {
        val setScriptTx = mkSetAccountScript(receiver, script = Some(ExprScript(Terms.TRUE).explicitGet()))
        broadcastAndAwait(setScriptTx)

        wait(client.hasScript(receiver)) shouldBe true
      }

      withClue("run script") {
        val now  = System.currentTimeMillis()
        val pair = AssetPair.createAssetPair(UsdId.toString, "WAVES").get
        val buy  = Order.buy(bob, matcher, pair, 1L, 2 * Order.PriceConstant, now, now + 1.day.toMillis, 0)

        wait(client.runScript(receiver, buy)) shouldBe RunScriptResult.Allowed
      }
    }
  }

  "spendableBalance" in {
    wait(client.spendableBalance(bob, Waves)) shouldBe 494994799299998L
    wait(client.spendableBalance(bob, randomIssuedAsset)) shouldBe 0L
  }

  "forgedOrder" - {
    "no such order" in {
      wait(client.forgedOrder(randomByteStr(32))) shouldBe false
    }

    "the order was in a forged ExchangeTransaction" in {
      val pair       = AssetPair.createAssetPair(UsdId.toString, "WAVES").get
      val exchangeTx = mkExchange(bob, alice, pair, 1L, 2 * Order.PriceConstant, matcher = matcher)

      broadcastAndAwait(exchangeTx)

      wait(client.forgedOrder(exchangeTx.buyOrder.id())) shouldBe true
      wait(client.forgedOrder(exchangeTx.sellOrder.id())) shouldBe true
    }
  }

  // TODO check that the functions returns new data after the state is changed?

  private def wait[T](f: => Future[T]): T = Await.result(f, 10.seconds)

  private def randomByteStr(len: Int): ByteStr = {
    val inner = new Array[Byte](len)
    Random.nextBytes(inner)
    ByteStr(inner)
  }

  private def randomIssuedAsset: IssuedAsset = IssuedAsset(randomByteStr(32))
}
