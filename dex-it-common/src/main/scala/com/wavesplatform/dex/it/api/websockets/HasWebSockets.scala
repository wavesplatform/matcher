package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.websockets.{WsBalances, WsMessage, WsOrder, WsOrderBook}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.config.PredefinedAssets
import com.wavesplatform.dex.it.docker.{DexContainer, apiKey}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import mouse.any._
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.reflect.ClassTag

trait HasWebSockets extends BeforeAndAfterAll { _: Suite with Eventually with Matchers with DiffMatcherWithImplicits with PredefinedAssets =>

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)
  implicit protected val efc: ErrorFormatterContext = assetDecimalsMap.apply

  protected val authenticatedStreamSignaturePrefix = "au"

  protected def getBaseBalancesStreamUri(dex: DexContainer): String   = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/accountUpdates/"
  protected def getBaseOrderBooksStreamUri(dex: DexContainer): String = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/orderbook/"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection[_]]()

  protected def addConnection(connection: WsConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWsAuthenticatedConnection(client: KeyPair,
                                            dex: DexContainer,
                                            keepAlive: Boolean = true,
                                            connectionLifetime: FiniteDuration = 1.hour): WsAuthenticatedConnection = {

    val timestamp     = System.currentTimeMillis() + connectionLifetime.toMillis
    val signedMessage = authenticatedStreamSignaturePrefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"${getBaseBalancesStreamUri(dex)}${client.publicKey.toAddress}?p=${client.publicKey}&t=$timestamp&s=$signature"

    new WsAuthenticatedConnection(wsUri, None, keepAlive) unsafeTap addConnection
  }

  protected def mkWsAuthenticatedConnectionViaApiKey(client: KeyPair,
                                                     dex: DexContainer,
                                                     apiKey: String = apiKey,
                                                     keepAlive: Boolean = true): WsAuthenticatedConnection = {
    val wsUri = s"${getBaseBalancesStreamUri(dex)}${client.publicKey.toAddress}"
    new WsAuthenticatedConnection(wsUri, Some(apiKey), keepAlive) unsafeTap addConnection
  }

  protected def mkWsOrderBookConnection(assetPair: AssetPair, dex: DexContainer): WsConnection[WsOrderBook] = {
    val wsUri = s"${getBaseOrderBooksStreamUri(dex)}${assetPair.amountAssetStr}/${assetPair.priceAssetStr}"
    mkWsConnection(wsUri) { msg =>
      Json.parse(msg.asTextMessage.getStrictText).as[WsOrderBook]
    }
  }

  protected def mkWsConnection[Output <: WsMessage: ClassTag](uri: String)(parseOutput: Message => Output): WsConnection[Output] = {
    new WsConnection(uri, parseOutput, trackOutput = true) unsafeTap addConnection
  }

  protected def assertChanges(c: WsAuthenticatedConnection, squash: Boolean = true)(expBs: Map[Asset, WsBalances]*)(expOs: WsOrder*): Unit = {

    def squashBalances(bs: Seq[Map[Asset, WsBalances]]): Map[Asset, WsBalances] = bs.foldLeft(Map.empty[Asset, WsBalances])(_ ++ _)

    def squashOrders(os: Seq[WsOrder]): Seq[WsOrder] = {
      os.groupBy(_.id)
        .mapValues { orderChanges =>
          orderChanges
            .foldLeft(orderChanges.head) {
              case (acc, oc) =>
                acc.copy(status = oc.status, filledAmount = oc.filledAmount, filledFee = oc.filledFee, avgWeighedPrice = oc.avgWeighedPrice)
            }
        }
        .values
        .toSeq
    }

    eventually {
      if (squash) {
        //        c.getBalancesChanges.size should be <= expBs.size // TODO Return after DEX-717
        squashBalances(c.getBalancesChanges) should matchTo { squashBalances(expBs) }
        //        c.getOrderChanges.size should be <= expOs.size // TODO Return after DEX-717
        squashOrders(c.getOrderChanges) should matchTo { squashOrders(expOs) }
      } else {
        c.getBalancesChanges should matchTo(expBs)
        c.getOrderChanges should matchTo(expOs)
      }
    }

    c.clearMessagesBuffer()
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
