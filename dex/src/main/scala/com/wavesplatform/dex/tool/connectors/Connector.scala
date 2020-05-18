package com.wavesplatform.dex.tool.connectors

trait Connector extends AutoCloseable {
  val target: String
}
