package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOrJsonResponse, RepeatRequestOptions}
import com.wavesplatform.wavesj.Transaction
import com.wavesplatform.wavesj.json.WavesJsonMapper
import play.api.libs.json.jackson.PlayJsonModule
import play.api.libs.json.{JsValue, JsonParserSettings}
import sttp.client._
import sttp.model.MediaType

import scala.concurrent.duration._

case class NodeRestConnector(target: String, chainId: Byte) extends RestConnector {

  private val mapper: WavesJsonMapper = new WavesJsonMapper(chainId); mapper.registerModule(new PlayJsonModule(JsonParserSettings()))

  def broadcastTx(tx: Transaction): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$target/transactions/broadcast").body(mapper writeValueAsString tx).contentType(MediaType.ApplicationJson)
  }

  def getTxInfo(txId: String): ErrorOrJsonResponse    = mkResponse { _.get(uri"$target/transactions/info/$txId") }
  def getTxInfo(tx: JsValue): ErrorOrJsonResponse     = getTxInfo { (tx \ "id").as[String] }
  def getTxInfo(tx: Transaction): ErrorOrJsonResponse = getTxInfo(tx.getId.toString)

  override val repeatRequestOptions: RestConnector.RepeatRequestOptions = RepeatRequestOptions(30, 1.second)
}
