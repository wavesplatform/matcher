package com.wavesplatform.dex.tool.connectors

import cats.syntax.option._
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.tool.connectors.RestConnector.ErrorOrJsonResponse
import play.api.libs.json.{JsValue, Json}
import sttp.client3._
import sttp.model.MediaType
import sttp.model.Uri.QuerySegment

case class DexRestConnector(target: String) extends RestConnector {

  private val apiUri = s"$targetUri/matcher"

  private def mkCancelRequest(orderId: Order.Id, owner: KeyPair): HttpCancelOrder = {
    val cancelRequest = HttpCancelOrder(owner, orderId.some, None, Array.emptyByteArray)
    val signature = crypto.sign(owner, cancelRequest.toSign)
    cancelRequest.copy(signature = signature)
  }

  private def cancelOrdersByRequest(cancelRequest: HttpCancelOrder, assetPair: AssetPair): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$apiUri/orderbook/${assetPair.amountAsset}/${assetPair.priceAsset}/cancel")
      .body(Json.stringify(Json toJson cancelRequest))
      .contentType(MediaType.ApplicationJson)
  }

  private def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
  )

  def placeOrder(order: Order): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$apiUri/orderbook").body(order.jsonStr).contentType(MediaType.ApplicationJson)
  }

  def cancelOrder(orderId: Order.Id, assetPair: AssetPair, owner: KeyPair): ErrorOrJsonResponse =
    cancelOrdersByRequest(mkCancelRequest(orderId, owner), assetPair)

  def cancelOrder(order: Order, owner: KeyPair): ErrorOrJsonResponse = cancelOrder(order.id(), order.assetPair, owner)

  def getOrderStatus(orderId: Order.Id, assetPair: AssetPair): ErrorOrJsonResponse = mkResponse {
    _.get(uri"$apiUri/orderbook/${assetPair.amountAsset}/${assetPair.priceAsset}/$orderId")
  }

  def getTxsByOrderId(id: Order.Id): ErrorOr[Seq[JsValue]] = mkResponse(_.get(uri"$apiUri/transactions/$id")) map { _.as[Seq[JsValue]] }

  def waitForOrderStatus(orderId: Order.Id, assetPair: AssetPair, expectedStatusName: String): ErrorOrJsonResponse =
    repeatRequest(getOrderStatus(orderId, assetPair)) {
      _.map(json => (json \ "status").get.asOpt[String] contains expectedStatusName).getOrElse(false)
    }

  def waitForOrderStatus(order: Order, expectedStatusName: String): ErrorOrJsonResponse =
    waitForOrderStatus(order.id(), order.assetPair, expectedStatusName)

  def getMatcherStatus(apiKey: String): ErrorOrJsonResponse = mkResponse {
    _.get(uri"$apiUri/debug/status").header("X-Api-Key", apiKey)
  }

  def getActiveOrdersByPair(keyPair: KeyPair, assetPair: AssetPair): ErrorOr[Seq[JsValue]] = {
    val uri =
      uri"$apiUri/orderbook/${assetPair.amountAsset}/${assetPair.priceAsset}/publicKey/${keyPair.publicKey.toString}"
        .copy(querySegments = List(QuerySegment.KeyValue("activeOnly", "true")))
    mkResponse(_.get(uri).headers(timestampAndSignatureHeaders(keyPair, System.currentTimeMillis))).map(_.as[Seq[JsValue]])
  }

  def getMatcherSettings: ErrorOr[JsValue] = mkResponse(_.get(uri"$apiUri/settings"))
}
