package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.tool.connectors.Connector.RepeatRequestOptions
import com.wavesplatform.dex.tool.connectors.RestConnector.ErrorOrJsonResponse
import com.wavesplatform.wavesj.Transaction
import com.wavesplatform.wavesj.json.WavesJsonMapper
import play.api.libs.json.jackson.PlayJsonModule
import play.api.libs.json.{JsValue, JsonParserSettings}
import sttp.client._
import sttp.model.MediaType

import scala.annotation.tailrec
import scala.concurrent.duration._

case class NodeRestConnector(target: String, chainId: Byte, timeBetweenBlocks: FiniteDuration) extends RestConnector {

  override implicit val repeatRequestOptions: RepeatRequestOptions = RepeatRequestOptions((timeBetweenBlocks * 1.5).toSeconds.toInt, 1.second)

  private val mapper: WavesJsonMapper = new WavesJsonMapper(chainId); mapper.registerModule(new PlayJsonModule(JsonParserSettings()))

  def broadcastTx(tx: Transaction): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$targetUri/transactions/broadcast").body(mapper writeValueAsString tx).contentType(MediaType.ApplicationJson)
  }

  def getTxInfo(txId: String): ErrorOrJsonResponse    = mkResponse { _.get(uri"$targetUri/transactions/info/$txId") }
  def getTxInfo(tx: JsValue): ErrorOrJsonResponse     = getTxInfo { (tx \ "id").as[String] }
  def getTxInfo(tx: Transaction): ErrorOrJsonResponse = getTxInfo(tx.getId.toString)

  def getCurrentHeight: ErrorOr[Long] = mkResponse { _.get(uri"$targetUri/blocks/height") }.map(json => (json \ "height").as[Long])

  @tailrec
  final def waitForHeightArise(): ErrorOr[Long] = getCurrentHeight match {
    case Right(origHeight) => repeatRequest(getCurrentHeight) { _.exists(_ > origHeight) }
    case Left(_)           => waitForHeightArise()
  }
}
