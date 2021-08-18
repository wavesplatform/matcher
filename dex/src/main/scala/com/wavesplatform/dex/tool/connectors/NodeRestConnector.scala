package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.tool.connectors.Connector.RepeatRequestOptions
import com.wavesplatform.dex.tool.connectors.RestConnector.ErrorOrJsonResponse
import com.wavesplatform.transactions.Transaction
import play.api.libs.json.JsValue
import sttp.client3._
import sttp.model.MediaType

import scala.annotation.tailrec
import scala.concurrent.duration._

case class NodeRestConnector(target: String, chainId: Byte) extends RestConnector {

  implicit override val repeatRequestOptions: RepeatRequestOptions = {
    val blocksCount = 5
    (
      for {
        currentHeight <- getCurrentHeight
        blocksTs <- getBlockHeadersAtHeightRange((currentHeight - blocksCount).max(0), currentHeight)
          .map(_.map(json => (json \ "timestamp").as[Long]))
          .ensure("0 or 1 blocks have been forged at the moment, try again later")(_.size > 1)
      } yield {
        val timeBetweenBlocks = ((blocksTs.last - blocksTs.head) / blocksCount * 1.5 / 1000).toInt
        RepeatRequestOptions(timeBetweenBlocks, 1.second)
      }
    ).fold(ex => throw new RuntimeException(s"Could not construct repeat request options: $ex"), identity)
  }

  def broadcastTx(tx: Transaction): ErrorOrJsonResponse = mkResponse {
    _.post(uri"$targetUri/transactions/broadcast").body(tx.toJson).contentType(MediaType.ApplicationJson)
  }

  def getTxInfo(txId: String): ErrorOrJsonResponse = mkResponse(_.get(uri"$targetUri/transactions/info/$txId"))
  def getTxInfo(tx: JsValue): ErrorOrJsonResponse = getTxInfo((tx \ "id").as[String])
  def getTxInfo(tx: Transaction): ErrorOrJsonResponse = getTxInfo(tx.id.toString)

  def getCurrentHeight: ErrorOr[Long] = mkResponse(_.get(uri"$targetUri/blocks/height")).map(json => (json \ "height").as[Long])

  def getBlockHeadersAtHeightRange(from: Long, to: Long): ErrorOr[Seq[JsValue]] =
    mkResponse(_.get(uri"$targetUri/blocks/headers/seq/$from/$to")).map(_.as[Seq[JsValue]])

  @tailrec
  final def waitForHeightArise(): ErrorOr[Long] = getCurrentHeight match {
    case Right(origHeight) => repeatRequest(getCurrentHeight)(_.exists(_ > origHeight))
    case Left(_) => waitForHeightArise()
  }

}
