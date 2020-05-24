package com.wavesplatform.dex.load.ws

import com.github.andyglow.websocket._
import com.github.andyglow.websocket.util.Uri
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsOrder, WsOrderBook}
import com.wavesplatform.dex.domain.asset.AssetPair
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.collection.mutable
import scala.reflect.ClassTag

class WsApiClient(apiUri: String, address: String, aus: String, obs: Seq[String]) extends AutoCloseable {

  private val log = LoggerFactory.getLogger(s"WsApiClient[$address]")

  private var accountUpdates   = WsAddressState(Map.empty, Seq.empty, 0L)
  private val orderBookUpdates = mutable.AnyRefMap.empty[AssetPair, WsOrderBook]

  private val protocolHandler = new WebsocketHandler[String]() {
    def receive: PartialFunction[String, Unit] = {
      case raw =>
        val json = unsafeDowncast[JsObject](Json.parse(raw))
        (json \ "T").as[String] match {
          case "pp" => sender() ! raw
          case "au" => accountUpdates = merge(accountUpdates, json.as[WsAddressState])
          case "ob" =>
            val diff = json.as[WsOrderBook]
            val updatedOb = orderBookUpdates.get(diff.assetPair) match {
              case Some(origOb) => merge(origOb, diff)
              case None         => diff
            }
            orderBookUpdates.put(diff.assetPair, updatedOb)
        }
    }

    override def onFailure: PartialFunction[Throwable, Unit] = {
      case e: Throwable => log.error("Got error", e)
    }

    override def onClose: Unit => Unit = _ => log.info("Closed")
  }

  private def merge(orig: WsAddressState, diff: WsAddressState): WsAddressState = WsAddressState(
    balances = orig.balances ++ diff.balances,
    orders = diff.orders.foldLeft(orig.orders) {
      case (r, x) =>
        val index = r.indexWhere(_.id == x.id)
        if (index < 0) x +: r
        else r.updated(index, merge(r(index), x))
    },
    updateId = diff.updateId,
    timestamp = diff.timestamp
  )

  private def merge(orig: WsOrder, diff: WsOrder): WsOrder = WsOrder(
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
    status = orig.status.orElse(diff.status),
    filledAmount = orig.filledAmount.orElse(diff.filledAmount),
    filledFee = orig.filledFee.orElse(diff.filledFee),
    avgWeighedPrice = orig.avgWeighedPrice.orElse(diff.avgWeighedPrice)
  )

  private def merge(orig: WsOrderBook, diff: WsOrderBook): WsOrderBook = WsOrderBook(
    assetPair = orig.assetPair,
    asks = orig.asks ++ diff.asks,
    bids = orig.bids ++ diff.bids,
    lastTrade = orig.lastTrade.orElse(diff.lastTrade),
    updateId = diff.updateId,
    settings = orig.settings.orElse(diff.settings), // TODO here merge required, but it wasn't used in these checks
    timestamp = diff.timestamp
  )

  private def unsafeDowncast[T <: JsValue](x: JsValue)(implicit ct: ClassTag[T]): T = x match {
    case x: T => x
    case _    => throw new RuntimeException(s"Expected an ${ct.runtimeClass.getName}, but got: $x")
  }

  private val client = WebsocketClient(Uri(apiUri), protocolHandler)
  private val socket = client.open()

  def run(): Unit = {
    socket ! aus
    obs.foreach(socket ! _)
  }

  override def close(): Unit = client.shutdownSync()
}
