package com.wavesplatform.dex.load.utils

case class Asset(quantity: Long, issueFee: Long, count: Int, pairsCount: Int)
case class Defaults(minimalOrderAmount: Long,
                    minimalOrderPrice: Long,
                    matcherFee: Long,
                    pairsFile: String,
                    maxOrdersPerAccount: Int,
                    wavesPerAccount: Long,
                    massTransferFee: Long,
                    massTransferMultiplier: Long)
case class Hosts(node: String, matcher: String, shooted: String)
case class Distribution(orderBookByPair: Double,
                        orderStatus: Double,
                        orderBookByPairAndKey: Double,
                        tradableBalance: Double,
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
