package com.wavesplatform.it

import java.net.InetSocketAddress

import cats.Id
import cats.instances.try_._
import com.softwaremill.sttp._
import com.typesafe.config.Config
import com.wavesplatform.it.api.{DexApi, NodeApi}
import com.wavesplatform.it.docker.{DexContainer, ItDockerClient, NodeContainer}
import com.wavesplatform.it.util._
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.util.Try

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
  private implicit val httpBackend            = HttpURLConnectionBackend()
  private implicit val tryHttpBackend         = TryHttpURLConnectionBackend()

  // Todo move to default config
  val smartFee         = 0.004.waves
  val minFee           = 0.001.waves + smartFee
  val issueFee         = 1.waves
  val smartIssueFee    = 1.waves + smartFee
  val leasingFee       = 0.002.waves + smartFee
  val tradeFee         = 0.003.waves
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected val dockerClient: ItDockerClient.type = ItDockerClient

  protected def wavesApi: NodeApi[Id]   = NodeApi.sync(NodeApi.async(wavesContainer()))
  protected val wavesNodeConfig: Config = ???
  protected val wavesContainer: Coeval[NodeContainer] = Coeval.evalOnce {
    dockerClient.createNode(wavesNodeConfig)
  }

  protected def getDexApiAddress: InetSocketAddress = dockerClient

  protected def dexApi: DexApi[Id]    = DexApi.unWrapped(DexApi[Try](getDexApiAddress))
  protected val dexNodeConfig: Config = ???
  protected val dexContainer: Coeval[DexContainer] = Coeval.evalOnce {
    dockerClient.createDex(dexNodeConfig)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    List(wavesContainer, dexContainer).foreach(x => dockerClient.start(x()))
  }

  override protected def afterAll(): Unit = {
    List(wavesContainer, dexContainer).foreach(x => dockerClient.stop(x()))
    super.afterAll()
  }
}
