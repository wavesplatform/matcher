package com.wavesplatform.it

import java.nio.charset.StandardCharsets

import cats.instances.FutureInstances
import com.wavesplatform.dex.asset.DoubleOps
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDex
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.matchers.ItMatchers
import com.wavesplatform.dex.it.test.InformativeTestStart
import com.wavesplatform.dex.it.waves.{MkWavesEntities, ToWavesJConversions}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.waves.WavesFeeConstants
import com.wavesplatform.it.api.ApiExtensions
import org.scalatest.concurrent.Eventually
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, CancelAfterFailure}

import scala.concurrent.duration.DurationInt

trait MatcherSuiteBase
    extends AnyFreeSpec
    with Matchers
    with CancelAfterFailure
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
    with ToWavesJConversions
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  override protected val moduleName: String = "dex-it"

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 30.seconds, interval = 1.second)

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
    stopBaseContainers()
    super.afterAll()
  }

  def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        assert(
          wavesNode1.api.balance(alice, asset) >= balance,
          s"Bob doesn't have enough balance in ${asset.toString} to make a transfer"
        )
        broadcastAndAwait(mkTransfer(alice, account.toAddress, balance, asset))
      }
    }
    account
  }
}
