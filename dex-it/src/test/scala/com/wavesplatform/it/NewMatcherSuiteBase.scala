package com.wavesplatform.it

import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDEX
import com.wavesplatform.dex.it.api.node.NewHasWavesNode
import com.wavesplatform.dex.it.assets.DoubleOps
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.docker._
import com.wavesplatform.dex.it.matchers.ItMatchers
import com.wavesplatform.dex.it.waves.{MkWavesEntities, WavesFeeConstants}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.it.api.NewApiExtensions
import com.wavesplatform.utils.ScorexLogging
import mouse.any._
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

abstract class NewMatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with BaseContainersKit
    with HasDEX
    with NewHasWavesNode
    with MkWavesEntities
    with NewApiExtensions
    with ItMatchers
    with NewDockerExtensions
    with DoubleOps
    with WavesFeeConstants
    with PredefinedAssets
    with PredefinedAccounts
    with DiffMatcherWithImplicits
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  override protected val moduleName: String = "dex-it"

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 30.seconds, interval = 1.second)

  protected override def invalidateCaches(): Unit = {
    invalidateDEXCaches()
    invalidateWavesNodeCaches()
  }

  override protected def beforeAll(): Unit = {
    log.debug(s"Perform beforeAll")
    startAndWait(wavesNodeContainer(), wavesNodeApi)
    startAndWait(dexContainer(), dexApi)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Perform afterAll")
    stopBaseContainers()
    super.afterAll()
  }

  override protected def runTest(testName: String, args: Args): Status = {

    def print(text: String): Unit = {
      val formatted = s"---------- $text ----------"
      log.debug(formatted)
      knownContainers.asScala.foreach { _.printDebugMessage(formatted) }
    }

    print(s"Test '$testName' started")

    super.runTest(testName, args) unsafeTap {
      _.whenCompleted {
        case Success(r) => print(s"Test '$testName' ${if (r) "succeeded" else "failed"}")
        case Failure(e) => print(s"Test '$testName' failed with exception '${e.getClass.getSimpleName}'")
      }
    }
  }
}
