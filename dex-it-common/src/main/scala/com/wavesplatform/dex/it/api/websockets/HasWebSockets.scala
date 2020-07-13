package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.wavesplatform.dex.api.ws.connection.{WsConnection, WsConnectionOps}
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressSubscribe, WsInitial, WsOrderBookSubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.config.PredefinedAssets
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import mouse.any._
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}

import scala.concurrent.duration._

trait HasWebSockets extends BeforeAndAfterAll with BeforeAndAfterEach with HasJwt with WsConnectionOps with WsMessageOps {
  _: Suite with Eventually with Matchers with DiffMatcherWithImplicits with PredefinedAssets =>

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)
  implicit protected val efc: ErrorFormatterContext = ErrorFormatterContext.from(assetDecimalsMap)

  protected def getWsStreamUri(dex: DexContainer): String = s"ws://127.0.0.1:${dex.restApiAddress.getPort}/ws/v0"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection, lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection]()

  protected def addConnection(connection: WsConnection): Unit = knownWsConnections.add(connection)

  protected def mkWsAddressConnection(client: KeyPair,
                                      dex: DexContainer,
                                      keepAlive: Boolean = true,
                                      subscriptionLifetime: FiniteDuration = 1.hour): WsConnection = {
    val jwt        = mkJwt(client, lifetime = subscriptionLifetime)
    val connection = mkDexWsConnection(dex, keepAlive)
    connection.send(WsAddressSubscribe(client.toAddress, WsAddressSubscribe.defaultAuthType, jwt))
    connection
  }

  protected def mkWsOrderBookConnection(assetPair: AssetPair, dex: DexContainer, depth: Int = 1): WsConnection = {
    val connection = mkDexWsConnection(dex)
    connection.send(WsOrderBookSubscribe(assetPair, depth))
    connection
  }

  protected def mkWsInternalConnection(dex: DexContainer, keepAlive: Boolean = true): WsConnection =
    mkWsConnection(s"${getWsStreamUri(dex)}/internal", keepAlive)

  protected def mkDexWsConnection(dex: DexContainer, keepAlive: Boolean = true): WsConnection =
    mkWsConnection(getWsStreamUri(dex), keepAlive)

  protected def mkWsConnection(uri: String, keepAlive: Boolean = true): WsConnection = {
    new WsConnection(uri, keepAlive) unsafeTap { wsc =>
      addConnection(wsc)
      eventually { wsc.collectMessages[WsInitial] should have size 1 }
      wsc.clearMessages()
    }
  }

  protected def assertChanges(c: WsConnection, squash: Boolean = true)(expBs: Map[Asset, WsBalances]*)(expOs: WsOrder*): Unit = {
    eventually {
      if (squash) {
        c.balanceChanges.size should be <= expBs.size
        c.balanceChanges.squashed should matchTo { expBs.toList.squashed }
        c.orderChanges.size should be <= expOs.size
        c.orderChanges.squashed should matchTo { expOs.toList.squashed }
      } else {
        c.balanceChanges should matchTo(expBs)
        c.orderChanges should matchTo(expOs)
      }
    }

    c.clearMessages()
  }

  protected def cleanupWebSockets(): Unit = if (!knownWsConnections.isEmpty) {
    knownWsConnections.forEach { _.close() }
    knownWsConnections.clear()
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    cleanupWebSockets()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
    materializer.shutdown()
  }
}
