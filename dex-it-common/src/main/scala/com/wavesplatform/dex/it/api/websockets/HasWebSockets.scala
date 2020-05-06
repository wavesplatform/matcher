package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.wavesplatform.dex.api.websockets.{WsAddressSubscribe, WsBalances, WsOrder, WsOrderBookSubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.config.PredefinedAssets
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import mouse.any._
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._

trait HasWebSockets extends BeforeAndAfterAll with HasJwt with WsConnectionOps with WsMessageOps {
  _: Suite with Eventually with Matchers with DiffMatcherWithImplicits with PredefinedAssets =>

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)
  implicit protected val efc: ErrorFormatterContext = assetDecimalsMap.apply

  protected def getWsStreamUri(dex: DexContainer): String = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/v0"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection, lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection]()

  protected def addConnection(connection: WsConnection): Unit = knownWsConnections.add(connection)

  protected def mkWsAddressConnection(client: KeyPair,
                                      dex: DexContainer,
                                      keepAlive: Boolean = true,
                                      connectionLifetime: FiniteDuration = 1.hour): WsConnection = {
    val jwt        = mkJwt(mkJwtSignedPayload(client, lifetime = connectionLifetime))
    val connection = mkWsConnection(dex, keepAlive)
    connection.send(WsAddressSubscribe(client.toAddress, WsAddressSubscribe.defaultAuthType, jwt))
    connection
  }

  protected def mkWsOrderBookConnection(assetPair: AssetPair, dex: DexContainer, depth: Int = 1): WsConnection = {
    val connection = mkWsConnection(dex)
    connection.send(WsOrderBookSubscribe(assetPair, depth))
    connection
  }

  protected def mkWsConnection(dex: DexContainer, keepAlive: Boolean = true): WsConnection =
    new WsConnection(getWsStreamUri(dex), keepAlive) unsafeTap addConnection

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

  protected def cleanupWebSockets(): Unit = {
    if (!knownWsConnections.isEmpty) {
      knownWsConnections.forEach { _.close() }
      materializer.shutdown()
    }
  }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
  }
}
