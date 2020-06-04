package com.wavesplatform.dex.load

case class Asset(quantity: Long, issueFee: Long)
case class Hosts(node: String, matcher: String, shooted: String)

case class Settings(
    networkByte: String,
    richAccount: String,
    dexRestApiKey: String,
    matcherPublicKey: String,
    assets: Asset,
    distribution: Map[String, Double],
    hosts: Hosts
)
