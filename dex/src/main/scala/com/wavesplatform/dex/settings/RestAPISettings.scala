package com.wavesplatform.dex.settings

case class RestAPISettings(address: String, port: Int, apiKeyHashes: List[String], cors: Boolean, apiKeyDifferentHost: Boolean)
