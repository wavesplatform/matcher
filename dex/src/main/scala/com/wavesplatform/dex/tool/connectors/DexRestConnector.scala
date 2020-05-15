package com.wavesplatform.dex.tool.connectors

import sttp.client._

private[tool] case class DexRestConnector(target: String) extends RestConnector {
  def swaggerRequest: Identity[Response[Either[String, String]]] = basicRequest.get(uri"$target/api-docs/swagger.json").send()
}
