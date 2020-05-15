package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.dex.api.CancelOrderRequest
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import play.api.libs.json.Json
import sttp.client._
import sttp.model.MediaType

private[tool] case class DexRestConnector(target: String) extends RestConnector {

  private val apiUri = s"$target/matcher"

  def swaggerRequest: Identity[Response[Either[String, String]]] = basicRequest.get(uri"$target/api-docs/swagger.json").send()

  def placeOrder(order: Order): Identity[Response[Either[String, String]]] =
    basicRequest.post(uri"$apiUri/orderbook").body(order.jsonStr).contentType(MediaType.ApplicationJson).send()

  def cancelOrder(order: Order, owner: KeyPair): Identity[Response[Either[String, String]]] = {

    val cancelOrderRequest = mkCancelRequest(order, owner)
    val body               = Json.stringify(Json.toJson(cancelOrderRequest))

    basicRequest
      .post(uri"$apiUri/orderbook/${order.assetPair.amountAsset}/${order.assetPair.priceAsset}/cancel")
      .body(body)
      .contentType(MediaType.ApplicationJson)
      .send()
  }

  def getOrderStatus(order: Order): Identity[Response[Either[String, String]]] =
    basicRequest.get(uri"$apiUri/orderbook/${order.assetPair.amountAsset}/${order.assetPair.priceAsset}/${order.id()}").send()

  private def mkCancelRequest(order: Order, owner: KeyPair): CancelOrderRequest = {
    val cancelRequest = CancelOrderRequest(owner, Some(ByteStr.decodeBase58(order.id().toString).get), None, Array.emptyByteArray)
    val signature     = crypto.sign(owner, cancelRequest.toSign)
    cancelRequest.copy(signature = signature)
  }
}
