package com.wavesplatform.dex.tool.connectors

trait Connector {
  val target: String
  def close(): Unit
}
