package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.dex.api.CancelOrderRequest
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.tool.ErrorOr
import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOrJsonResponse, RepeatRequestOptions}
import play.api.libs.json.{JsValue, Json}
import sttp.client._
import sttp.model.MediaType

import scala.concurrent.duration._

case class DexRestConnector(target: String) extends RestConnector {

  private val apiUri = s"$target/matcher"

  private def mkCancelRequest(order: Order, owner: KeyPair): CancelOrderRequest = {
    val cancelRequest = CancelOrderRequest(owner, Some(ByteStr.decodeBase58(order.id().toString).get), None, Array.emptyByteArray)
    val signature     = crypto.sign(owner, cancelRequest.toSign)
    cancelRequest.copy(signature = signature)
  }

  def placeOrder(order: Order): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$apiUri/orderbook").body(order.jsonStr).contentType(MediaType.ApplicationJson)
  }

  def cancelOrder(order: Order, owner: KeyPair): ErrorOrJsonResponse = {
    val cancelOrderRequest = mkCancelRequest(order, owner)
    val body               = Json.stringify(Json toJson cancelOrderRequest)
    mkResponse {
      _.post(uri"$apiUri/orderbook/${order.assetPair.amountAsset}/${order.assetPair.priceAsset}/cancel")
        .body(body)
        .contentType(MediaType.ApplicationJson)
    }
  }

  def getOrderStatus(order: Order): ErrorOrJsonResponse = mkResponse {
    _.get(uri"$apiUri/orderbook/${order.assetPair.amountAsset}/${order.assetPair.priceAsset}/${order.id()}")
  }

  def getTxsByOrderId(id: Order.Id): ErrorOr[Seq[JsValue]] = mkResponse { _.get(uri"$apiUri/transactions/$id") } map { _.as[Seq[JsValue]] }

  def waitForOrderStatus(order: Order, expectedStatusName: String): ErrorOrJsonResponse =
    repeatRequest { getOrderStatus(order) } { _.map(json => (json \ "status").get.asOpt[String] contains expectedStatusName).getOrElse(false) }

  override val repeatRequestOptions: RestConnector.RepeatRequestOptions = RepeatRequestOptions(10, 1.second)
}
