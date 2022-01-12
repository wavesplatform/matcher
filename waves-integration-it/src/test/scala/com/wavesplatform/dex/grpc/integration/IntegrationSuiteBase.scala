package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.node.NodeApiExtensions
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.it.containers.{BaseContainersKit, HasWavesNode}
import com.wavesplatform.dex.it.test.InformativeTestStart
import com.wavesplatform.dex.it.waves.{MkWavesEntities, ToWavesJConversions}
import com.wavesplatform.dex.predefined.{PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.util.DoubleOps
import com.wavesplatform.dex.utils.DiffMatcherWithImplicits
import com.wavesplatform.dex.waves.WavesFeeConstants
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.duration.DurationInt

trait IntegrationSuiteBase
    extends AnyFreeSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with BaseContainersKit
    with HasWavesNode
    with MkWavesEntities
    with WavesFeeConstants
    with NodeApiExtensions
    with PredefinedAssets
    with PredefinedAccounts
    with DoubleOps
    with DiffMatcherWithImplicits
    with InformativeTestStart
    with ToWavesJConversions
    with AllureScalatestContext
    with ScalaFutures
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  override protected val moduleName: String = "waves-integration-it"

  implicit override def patienceConfig: PatienceConfig = super.patienceConfig.copy(1.minute, 1.second)

  override protected def beforeAll(): Unit = {
    log.debug(s"Perform beforeAll")
    wavesNode1.start()
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Perform afterAll")
    stopBaseContainers()
    super.afterAll()
  }

  override protected def step(text: String): Unit = {
    info(text)
    super.step(text)
  }

}
