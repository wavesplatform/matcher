package com.wavesplatform.it

import cats.instances.FutureInstances
import cats.syntax.either._
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.http.entities.HttpV0OrderBook
import com.wavesplatform.dex.asset.DoubleOps
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDex
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.matchers.ItMatchers
import com.wavesplatform.dex.it.test.{InformativeTestStart, NoStackTraceCancelAfterFailure}
import com.wavesplatform.dex.it.waves.{MkWavesEntities, ToWavesJConversions}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.waves.WavesFeeConstants
import com.wavesplatform.it.api.ApiExtensions
import com.wavesplatform.transactions.ExchangeTransaction
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, OptionValues}

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.duration._

trait MatcherSuiteBase
    extends AnyFreeSpec
    with AllureScalatestContext
    with Matchers
    with NoStackTraceCancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with BaseContainersKit
    with HasDex
    with HasWavesNode
    with MkWavesEntities
    with ApiExtensions
    with ItMatchers
    with DoubleOps
    with WavesFeeConstants
    with PredefinedAssets
    with PredefinedAccounts
    with DiffMatcherWithImplicits
    with InformativeTestStart
    with FutureInstances
    with ScalaFutures
    with OptionValues
    with ToWavesJConversions
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  private val maybeTraceTickInterval =
    Option(System.getenv("CONFIG_FORCE_kamon_trace_tick__interval"))
      .flatMap(x => Either.catchNonFatal(Duration(x)).toOption)
      .filter(_ => Option(System.getenv("CONFIG_FORCE_kamon_modules_jaeger_enabled")).contains("true"))

  implicit val httpV0OrderBookDiff: Derived[Diff[HttpV0OrderBook]] = Derived(Diff.gen[HttpV0OrderBook].ignore[HttpV0OrderBook, Long](_.timestamp))
  implicit val exchangeTransactionDiff: Derived[Diff[ExchangeTransaction]] = Derived(Diff[String].contramap[ExchangeTransaction](_.id().base58))

  override protected val moduleName: String = "dex-it"

  implicit override def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 1.minute, interval = 1.second)

  override protected def beforeAll(): Unit = {
    log.debug(s"Perform beforeAll")
    kafkaServer.foreach { _ =>
      createKafkaTopic(dexRunConfig.getString("waves.dex.events-queue.kafka.topic"))
    }
    wavesNode1.start()
    dex1.start()
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Perform afterAll")
    maybeTraceTickInterval.map(_.toMillis + 2.seconds.toMillis).foreach(Thread.sleep)
    stopBaseContainers()
    super.afterAll()
  }

  def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = createAccountWithBalance(0, balances: _*)

  def createAccountWithBalance(index: Int, balances: (Long, Asset)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-$index-${ThreadLocalRandom.current().nextInt()}".getBytes(StandardCharsets.UTF_8)))

    val txIds = balances.map { case (balance, asset) =>
      assert(
        wavesNode1.api.balance(alice, asset) >= balance,
        s"Alice doesn't have enough balance in ${asset.toString} to make a transfer"
      )
      val tx = mkTransfer(alice, account.toAddress, balance, asset)
      wavesNode1.api.broadcast(tx)
      tx.id()
    }
    txIds.foreach(wavesNode1.api.waitForTransaction)
    account
  }

  protected def placeAndGetIds(count: Int): List[String] =
    (1 to count).map { i =>
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, i.usd)
      placeAndAwaitAtDex(o)
      o.idStr()
    }.toList

}
