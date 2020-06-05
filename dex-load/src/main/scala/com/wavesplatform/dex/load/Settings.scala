package com.wavesplatform.dex.load

case class Asset(quantity: Long, issueFee: Long, count: Int, pairsCount: Int)
case class Defaults(minimalOrderAmount: Long,
                    minimalOrderPrice: Long,
                    matcherFee: Long,
                    pairsFile: String,
                    maxOrdersPerAccount: Int,
                    wavesPerAccount: Long)
case class Hosts(node: String, matcher: String, shooted: String)
case class Distribution(orderHistoryByPair: Double,
                        orderStatus: Double,
                        orderHistoryByPairAndKey: Double,
                        tradableBalance: Double,
                        reservedBalance: Double,
                        placeOrder: Double,
                        cancelOrder: Double)

case class Settings(
    chainId: String,
    richAccount: String,
    dexRestApiKey: String,
    matcherPublicKey: String,
    assets: Asset,
    distribution: Distribution,
    defaults: Defaults,
    hosts: Hosts
)
