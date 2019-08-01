package com.wavesplatform.it

import java.net.{InetAddress, InetSocketAddress}
import java.util.Collections.singletonList

import cats.Id
import cats.instances.try_._
import com.google.common.primitives.Ints.toByteArray
import com.softwaremill.sttp._
import com.spotify.docker.client.messages.{Ipam, IpamConfig, Network, NetworkConfig}
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.Config
import com.wavesplatform.it.api.{DexApi, NodeApi}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, ItDockerClient, NodeContainer}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Random, Try}

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with ScorexLogging {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  private implicit val httpBackend: SttpBackend[Id, Nothing]     = HttpURLConnectionBackend()
  private implicit val tryHttpBackend: SttpBackend[Try, Nothing] = TryHttpURLConnectionBackend()

  private val dockerSpotifyClient = DefaultDockerClient.fromEnv().build()

  // a random network in 10.x.x.x range
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000

  // 10.x.x.x/28 network will accommodate up to 13 nodes
  private val networkPrefix = s"${InetAddress.getByAddress(toByteArray(networkSeed)).getHostAddress}/28"

  private val network = Coeval.evalOnce(mkNetwork(getClass.getSimpleName, s"waves-${Random.nextInt(Int.MaxValue)}"))

  protected val dockerClient: Coeval[ItDockerClient] = Coeval.evalOnce(new ItDockerClient(dockerSpotifyClient, network()))

  protected def getWavesApiAddress: InetSocketAddress =
    dockerClient().getInetSocketAddress(wavesContainer(), DexTestConfig.WavesNodeSettings.restAPISettings.port)

  private val grpcChannel = Coeval.evalOnce[ManagedChannel] {
    val h = getWavesApiAddress
    ManagedChannelBuilder.forAddress(h.getAddress.getHostAddress, h.getPort).usePlaintext(true).build()
  }

  protected def wavesApi: NodeApi[Id]   = NodeApi.sync(NodeApi.async(grpcChannel()))
  protected def wavesNodeConfig: Config = DexTestConfig.WavesNodeConfig
  protected val wavesContainer: Coeval[NodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode(wavesNodeConfig, ipForNode(1))
  }

  protected def getDexApiAddress: InetSocketAddress = dockerClient().getInetSocketAddress(dexContainer(), DexTestConfig.DexNodeSettings.restApi.port)
  protected def dexApi: DexApi[Id]                  = DexApi.unWrapped(DexApi[Try](getDexApiAddress))
  protected def dexNodeConfig: Config               = DexTestConfig.DexNodeConfig
  protected val dexContainer: Coeval[DexContainer] = Coeval.evalOnce {
    dockerClient().createDexNode(dexNodeConfig)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    network.apply()
    List(wavesContainer, dexContainer).foreach(x => dockerClient().start(x()))
  }

  override protected def afterAll(): Unit = {
    List(wavesContainer, dexContainer).foreach(x => dockerClient().stop(x()))
    super.afterAll()
  }

  private def ipForNode(nodeId: Int) = InetAddress.getByAddress(toByteArray(nodeId & 0xF | networkSeed)).getHostAddress
  private def mkNetwork(tag: String, networkName: String): Network = {
    def network: Option[Network] =
      try {
        val networks = dockerSpotifyClient.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
        if (networks.isEmpty) None else Some(networks.get(0))
      } catch {
        case NonFatal(_) => network
      }

    def attempt(rest: Int): Network =
      try {
        network match {
          case Some(n) =>
            val ipam = n
              .ipam()
              .config()
              .asScala
              .map(n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}")
              .mkString(", ")
            log.info(s"Network ${n.name()} (id: ${n.id()}) is created for $tag, ipam: $ipam")
            n
          case None =>
            log.debug(s"Creating network $networkName for $tag")
            // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
            val r = dockerSpotifyClient.createNetwork(
              NetworkConfig
                .builder()
                .name(networkName)
                .ipam(
                  Ipam
                    .builder()
                    .driver("default")
                    .config(singletonList(IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE))))
                    .build()
                )
                .checkDuplicate(true)
                .build())
            Option(r.warnings()).foreach(log.warn(_))
            attempt(rest - 1)
        }
      } catch {
        case NonFatal(e) =>
          log.warn(s"Can not create a network for $tag", e)
          if (rest == 0) throw e else attempt(rest - 1)
      }

    attempt(5)
  }
}
