package com.wavesplatform.dex.settings

case class RestAPISettings(address: String, port: Int, apiKeyHash: String, cors: Boolean, apiKeyDifferentHost: Boolean)
