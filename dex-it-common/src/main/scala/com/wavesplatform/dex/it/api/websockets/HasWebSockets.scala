package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import com.wavesplatform.dex.api.ws.connection.{WsConnection, WsConnectionOps}
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.fp.MapImplicits.MapOps
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

  implicit protected val system: ActorSystem = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)
  implicit protected val efc: ErrorFormatterContext = ErrorFormatterContext.from(assetDecimalsMap)

  protected def getWsStreamUri(dex: DexContainer, query: Map[String, String] = Map.empty): Uri =
    Uri
      .parseAbsolute(s"ws://127.0.0.1:${dex.restApiAddress.getPort}/ws/v0")
      .withQuery(Uri.Query(query))

  protected def getWsInternalStreamUri(dex: DexContainer): Uri =
    s"ws://127.0.0.1:${dex.restApiAddress.getPort}/ws/v0/internal"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection, lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection]()

  protected def addConnection(connection: WsConnection): Unit = knownWsConnections.add(connection)

  protected def mkWsAddressConnection(
    client: KeyPair,
    dex: DexContainer,
    keepAlive: Boolean = true,
    subscriptionLifetime: FiniteDuration = 1.hour
  ): WsConnection = {
    val jwt = mkJwt(client, lifetime = subscriptionLifetime)
    val connection = mkDexWsConnection(dex, keepAlive = keepAlive)
    connection.send(WsAddressSubscribe(client.toAddress, WsAddressSubscribe.defaultAuthType, jwt))
    connection
  }

  protected def mkWsOrderBookConnection(assetPair: AssetPair, dex: DexContainer, depth: Int = 1): WsConnection = {
    val connection = mkDexWsConnection(dex)
    connection.send(WsOrderBookSubscribe(assetPair, depth))
    connection
  }

  protected def mkWsRatesUpdatesConnection(dex: DexContainer): WsConnection = {
    val connection = mkDexWsConnection(dex)
    connection.send(WsRatesUpdatesSubscribe())
    connection
  }

  protected def mkWsInternalConnection(dex: DexContainer, keepAlive: Boolean = true): WsConnection =
    mkWsConnection(getWsInternalStreamUri(dex), keepAlive)

  protected def mkDexWsConnection(
    dex: DexContainer,
    os: Option[String] = None,
    client: Option[String] = None,
    keepAlive: Boolean = true
  ): WsConnection = {
    val query: Map[String, String] = Map
      .empty[String, String]
      .appendIfDefinedMany(
        "a_os" -> os,
        "a_client" -> client
      )

    mkWsConnection(getWsStreamUri(dex, query), keepAlive)
  }

  protected def mkWsConnection(uri: Uri, keepAlive: Boolean = true): WsConnection =
    new WsConnection(uri, keepAlive) unsafeTap { wsc =>
      addConnection(wsc)
      eventually(wsc.collectMessages[WsInitial] should have size 1)
      wsc.clearMessages()
    }

  protected def assertChanges(c: WsConnection, squash: Boolean = true)(expBs: Map[Asset, WsBalances]*)(expOs: WsOrder*): Unit = {
    eventually {
      if (squash) {
        c.balanceChanges.size should be <= expBs.size
        c.balanceChanges.squashed should matchTo(expBs.toList.squashed)
        c.orderChanges.size should be <= expOs.size
        c.orderChanges.squashed should matchTo(expOs.toList.squashed)
      } else {
        c.balanceChanges should matchTo(expBs)
        c.orderChanges should matchTo(expOs)
      }
    }

    c.clearMessages()
  }

  protected def mergeWsOrder(orig: WsOrder, diff: WsOrder): WsOrder = WsOrder(
    id = orig.id,
    timestamp = diff.timestamp,
    amountAsset = orig.amountAsset.orElse(diff.amountAsset),
    priceAsset = orig.priceAsset.orElse(diff.priceAsset),
    side = orig.side.orElse(diff.side),
    isMarket = orig.isMarket.orElse(diff.isMarket),
    price = orig.price.orElse(diff.price),
    amount = orig.amount.orElse(diff.amount),
    fee = orig.fee.orElse(diff.fee),
    feeAsset = orig.feeAsset.orElse(diff.feeAsset),
    status = diff.status.orElse(orig.status),
    filledAmount = diff.filledAmount.orElse(orig.filledAmount),
    filledFee = diff.filledFee.orElse(orig.filledFee),
    avgWeighedPrice = diff.avgWeighedPrice.orElse(orig.avgWeighedPrice),
    totalExecutedPriceAssets = diff.totalExecutedPriceAssets.orElse(orig.totalExecutedPriceAssets)
  )

  protected def mergeAddressChanges(orig: WsAddressChanges, diff: WsAddressChanges): WsAddressChanges = WsAddressChanges(
    address = diff.address,
    balances = orig.balances ++ diff.balances,
    orders = diff.orders.foldLeft(orig.orders) {
      case (r, x) =>
        val index = r.indexWhere(_.id == x.id)
        if (index < 0) x +: r
        else r.updated(index, mergeWsOrder(r(index), x))
    },
    updateId = diff.updateId,
    timestamp = diff.timestamp
  )

  protected def mergeOrderBookChanges(orig: WsOrderBookChanges, diff: WsOrderBookChanges): WsOrderBookChanges = WsOrderBookChanges(
    assetPair = orig.assetPair,
    asks = orig.asks ++ diff.asks,
    bids = orig.bids ++ diff.bids,
    lastTrade = orig.lastTrade.orElse(diff.lastTrade),
    updateId = diff.updateId,
    settings = orig.settings.orElse(diff.settings),
    timestamp = diff.timestamp
  )

  protected def cleanupWebSockets(): Unit = if (!knownWsConnections.isEmpty) {
    knownWsConnections.forEach(_.close())
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
