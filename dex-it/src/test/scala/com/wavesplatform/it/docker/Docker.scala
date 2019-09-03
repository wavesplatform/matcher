package com.wavesplatform.it.docker

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileOutputStream}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Collections._
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import cats.instances.map._
import cats.instances.string._
import cats.kernel.Monoid
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.primitives.Ints._
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.ConfigFactory._
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.{Random, Try}

class Docker(suiteName: String = "") extends AutoCloseable with ScorexLogging {

  import Docker._

  private val client          = DefaultDockerClient.fromEnv().build()
  private val knownContainers = ConcurrentHashMap.newKeySet[DockerContainer]()
  private val isStopped       = new AtomicBoolean(false)

  // a random network in 10.x.x.x range
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000

  // 10.x.x.x/28 network will accommodate up to 13 nodes
  private val networkPrefix = s"${InetAddress.getByAddress(toByteArray(networkSeed)).getHostAddress}/28"

  // A location for logs from containers on local machine
  private val logDir: Coeval[Path] = Coeval.evalOnce {
    val r = Option(System.getProperty("waves.it.logging.dir"))
      .map(Paths.get(_))
      .getOrElse(Paths.get(System.getProperty("user.dir"), "dex-it", "target", "logs", RunId, suiteName.replaceAll("""(\w)\w*\.""", "$1.")))

    Files.createDirectories(r)
    r
  }

  /**
    * @return The address inside the network
    */
  def getInternalSocketAddress(container: DockerContainer, internalPort: Int): InetSocketAddress = {
    val ns = client.inspectContainer(container.id).networkSettings()
    new InetSocketAddress(ns.networks().get(network().name()).ipAddress(), internalPort)
  }

  /**
    * @return The address outside the network, from host machine
    */
  def getExternalSocketAddress(container: DockerContainer, internalPort: Int): InetSocketAddress = {
    val ns = client.inspectContainer(container.id).networkSettings()
    val binding = Option(ns.ports().get(s"$internalPort/tcp"))
      .map(_.get(0))
      .getOrElse(throw new IllegalStateException(s"There is no mapping '$internalPort/tcp' for '${container.name}'"))

    new InetSocketAddress("127.0.0.1", binding.hostPort().toInt)
  }

  private def ipForNode(nodeId: Int) = InetAddress.getByAddress(toByteArray(nodeId & 0xF | networkSeed)).getHostAddress

  private val network: Coeval[Network] = Coeval.evalOnce {
    val id          = Random.nextInt(Int.MaxValue)
    val networkName = s"waves-$id"

    def network: Option[Network] =
      try {
        val networks = client.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
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
            log.info(s"Network '${n.name()}' (id: '${n.id()}') is created for '$suiteName', ipam: $ipam")
            n
          case None =>
            log.debug(s"Creating network '$networkName' for '$suiteName'")
            // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
            val r = client.createNetwork(
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
          log.warn(s"Can not create a network for $suiteName", e)
          if (rest == 0) throw e else attempt(rest - 1)
      }

    attempt(5)
  }

  def writeFile(container: DockerContainer, to: Path, content: String): Unit = {
    val os    = new ByteArrayOutputStream()
    val s     = new TarArchiveOutputStream(os)
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    val entry = new TarArchiveEntry(s"${to.getFileName}")
    entry.setSize(bytes.size)
    s.putArchiveEntry(entry)
    s.write(bytes)
    s.closeArchiveEntry()

    val is = new ByteArrayInputStream(os.toByteArray)
    s.close()

    try client.copyToContainer(is, container.id, s"${to.getParent.toString}")
    finally is.close()
  }

  def createWavesNode(name: String, config: Config): WavesNodeContainer = {
    val number   = getNumber(name)
    val basePath = "/opt/waves"
    val id = create(
      number,
      name,
      wavesNodeImage,
      Map(
        "WAVES_NODE_CONFIGPATH" -> s"$basePath/$name.conf",
        "WAVES_OPTS"            -> s"-Dlogback.configurationFile=$basePath/logback.xml"
      )
    )

    val r = new WavesNodeContainer(id, number, name, config, basePath)
    // Could not work in Windows
    writeFile(r, Paths.get(s"$basePath/$name.conf"), config.resolve().root().render())
    knownContainers.add(r)
    r
  }

  def createDex(name: String, config: Config): DexContainer = {
    val number = getNumber(name)
//    val grpc   = config.as[GRPCSettings]("waves.dex.waves-node-grpc")

    val allowedKeysPrefixes = List(
      "waves-node-grpc",
      "blacklisted",
      "allowed",
      "white-list-only",
      "price-assets",
      "rest-order-limit",
      "events-queue",
      "snapshots-interval",
      "matching-rules"
    )
    val props = renderProperties(asProperties(config.resolve().getConfig("waves.dex")).asScala.collect {
      case (key, v) if allowedKeysPrefixes.exists(key.startsWith) => s"waves.dex.$key" -> v
    }.toMap)

    val basePath = "/opt/waves-dex"
    val id = create(
      number,
      name,
      dexImage,
      Map(
        "WAVES_DEX_CONFIGPATH" -> s"$basePath/$name.conf",
        // -Dwaves.dex.waves-node-grpc.host=${grpc.host} -Dwaves.dex.waves-node-grpc.port=${grpc.port}
        "WAVES_DEX_OPTS" -> s"$props -Dlogback.configurationFile=$basePath/logback.xml"
      )
    )

    val r = new DexContainer(id, number, name, config, basePath)
    knownContainers.add(r)
    r
  }

  def start(container: DockerContainer): Unit = {
    log.debug(s"${prefix(container)} Starting ...")
    try client.startContainer(container.id)
    catch {
      case NonFatal(e) =>
        log.error(s"${prefix(container)} Can't start", e)
        throw e
    }
  }

  def stop(container: DockerContainer): Unit = {
    val containerInfo = client.inspectContainer(container.id)
    log.debug(s"""${prefix(container)} Information:
                 |Exit code: ${containerInfo.state().exitCode()}
                 |Error: ${containerInfo.state().error()}
                 |Status: ${containerInfo.state().status()}
                 |OOM killed: ${containerInfo.state().oomKilled()}""".stripMargin)

    log.debug(s"${prefix(container)} Stopping ...")
    try client.stopContainer(container.id, 10)
    catch {
      case NonFatal(e) =>
        log.error(s"${prefix(container)} Can't stop", e)
        throw e
    }

    saveLog(container)
  }

  def disconnectFromNetwork(container: DockerContainer): Unit = {
    log.debug(s"${prefix(container)} Disconnecting from network '${network().name()}' ...")
    client.disconnectFromNetwork(container.id, network().id())
    log.info(s"${prefix(container)} Disconnected from network '${network().name()}'")
  }

  def connectToNetwork(container: DockerContainer): Unit = {
    log.debug(s"${prefix(container)} Connecting to network '${network().name()}' ...")
    try client.connectToNetwork(
      network().id(),
      NetworkConnection
        .builder()
        .containerId(container.id)
        .endpointConfig(endpointConfigFor(container.number))
        .build()
    )
    catch {
      case NonFatal(e) =>
        log.error(s"${prefix(container)} Can't connect to the network '${network().name()}'", e)
        throw e
    }
  }

  def printDebugMessage(container: DockerContainer, text: String): Unit = {
    val escaped = text.replace('\'', '\"')
    val id      = client.execCreate(container.id, Array("/bin/sh", "-c", s"/bin/echo '$escaped' >> /proc/1/fd/1")).id()
    val exec    = client.execStart(id)
    try exec.readFully()
    catch { case NonFatal(e) => log.error(s"Can't print a debug message", e) } finally exec.close()
  }

  private def create(number: Int, name: String, imageName: String, env: Map[String, String]): String = {
    val ip            = ipForNode(number)
    val containerName = s"${network().name()}-$name"

    def info(id: String = "not yet created") = s"'$containerName': id='$id' name='$name', number='$number', image='$imageName', ip=$ip, env: $env"

    try {
      val containersWithSameName = client.listContainers(DockerClient.ListContainersParam.filter("name", containerName))
      if (!containersWithSameName.isEmpty) {
        dumpContainers(containersWithSameName, "Containers with the same name")
        throw new IllegalStateException(s"There is containers with the same name!")
      }

      val hostConfig = HostConfig
        .builder()
        .publishAllPorts(true)
        .build()

      // TODO
      val fixedEnv = Monoid.combine(
        env,
        Map("WAVES_OPTS" -> s" -Dwaves.network.declared-address=$ip:6883")
      )

      val containerConfig = ContainerConfig
        .builder()
        .image(imageName)
        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(network().name() -> endpointConfigFor(number)).asJava))
        .hostConfig(hostConfig)
        .env(fixedEnv.map { case (k, v) => s"$k=$v" }.toList.asJava)
        .build()

      log.debug(s"Creating container ${info()} ...")
      val r = client.createContainer(containerConfig, containerName)
      Option(r.warnings().asScala).toSeq.flatten.foreach(e => log.warn(s"""Error "$e", ${info(r.id())}"""))

      r.id()
    } catch {
      case NonFatal(e) =>
        log.error(s"Can't create a container ${info()}", e)
        dumpContainers(client.listContainers())
        throw e
    }
  }

  override def close(): Unit = if (isStopped.compareAndSet(false, true)) {
    log.info("Stopping containers")

    knownContainers.asScala.foreach { container =>
      stop(container)
      log.debug(s"${prefix(container)} Removing")
      try client.removeContainer(container.id)
      catch {
        case NonFatal(e) => log.warn(s"${prefix(container)} Can't remove", e)
      }
    }

    try {
      log.debug(s"Removing the '${network().id()}' network")
      client.removeNetwork(network().id())
    } catch {
      case NonFatal(e) =>
        // https://github.com/moby/moby/issues/17217
        log.warn(s"Can't remove the '${network().id()}' network", e)
    }

    client.close()
  }

  private def saveLog(container: DockerContainer): Unit = {
    val logFile = logDir().resolve(s"container-${container.name}.log").toFile
    log.info(s"${prefix(container)} Writing log to '${logFile.getAbsolutePath}'")

    val fileStream = new FileOutputStream(logFile, false)
    try {
      client
        .logs(
          container.id,
          DockerClient.LogsParam.follow(),
          DockerClient.LogsParam.stdout(),
          DockerClient.LogsParam.stderr()
        )
        .attach(fileStream, fileStream)
    } finally {
      fileStream.close()
    }
  }

  private def endpointConfigFor(number: Int): EndpointConfig = {
    val ip = ipForNode(number)
    EndpointConfig
      .builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()
  }

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x =
      if (containers.isEmpty) "No"
      else
        "\n" + containers.asScala
          .map { x =>
            s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})"
          }
          .mkString("\n")

    log.debug(s"$label: $x")
  }

  private def prefix(container: DockerContainer): String = s"[name='${container.name}', id=${container.id}]"

  private def getNumber(name: String): Int = {
    val raw = name
      .split('-')
      .lastOption
      .flatMap(x => Try(x.toInt).toOption)
      .getOrElse(throw new IllegalArgumentException(s"Can't parse the container's number: '$name'. It should have a form: <name>-<number>"))

    if (raw >= 5) throw new IllegalArgumentException("All slots are filled")
    else if (name.startsWith("dex-")) raw
    else if (name.startsWith("waves-")) raw + 5
    else throw new IllegalArgumentException(s"Can't parse number from '$name'. Know 'dex-' and 'waves-' only")
  }

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.debug("Shutdown hook")
    close()
  }
}

object Docker {
  private val wavesNodeImage = "com.wavesplatform/waves-integration-it:latest"
  private val dexImage       = "com.wavesplatform/dex-it:latest"

  private val RunId = Option(System.getenv("RUN_ID")).getOrElse(DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss").format(LocalDateTime.now()))

  private val jsonMapper  = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  val configTemplate: Config = parseResources("template.conf")

  def apply(owner: Class[_]): Docker = new Docker(suiteName = owner.getSimpleName)

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Map[String, String]): String =
    p.map {
        case (k, v) if v.contains(" ") => k -> s""""$v""""
        case x                         => x
      }
      .map { case (k, v) => s"-D$k=$v" }
      .mkString(" ")

}
