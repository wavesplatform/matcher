package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.wavesj.Transaction
import com.wavesplatform.wavesj.json.WavesJsonMapper
import play.api.libs.json.JsonParserSettings
import play.api.libs.json.jackson.PlayJsonModule
import sttp.client._
import sttp.model.MediaType

private[tool] case class NodeRestConnector(target: String, chainId: Byte) extends RestConnector {

  private val mapper: WavesJsonMapper = new WavesJsonMapper(chainId); mapper.registerModule(new PlayJsonModule(JsonParserSettings()))

  def broadcastTx(tx: Transaction): Identity[Response[Either[String, String]]] = {
    val serializedTx = mapper writeValueAsString tx
    basicRequest.post(uri"$target/transactions/broadcast").body(serializedTx).contentType(MediaType.ApplicationJson).send()
  }

  def txInfo(tx: Transaction): Identity[Response[Either[String, String]]] =
    basicRequest.get(uri"$target/transactions/info/${tx.getId.toString}").send()
}
