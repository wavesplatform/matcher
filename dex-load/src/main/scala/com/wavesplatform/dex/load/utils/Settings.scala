package com.wavesplatform.dex.load.utils

case class Asset(quantity: Long, issueFee: Long, count: Int, pairsCount: Int)

case class Defaults(
  minimalOrderAmount: Long,
  minimalOrderPrice: Long,
  matcherFee: Long,
  pairsFile: String,
  maxOrdersPerAccount: Int,
  wavesPerAccount: Long,
  massTransferFee: Long,
  massTransferMultiplier: Long
)

case class Hosts(node: String, matcher: String, shooted: String)

case class Distribution(
  orderBookByPair: Double = 0.65,
  orderStatus: Double = 0.15,
  orderBookByPairAndKey: Double = 0.33,
  tradableBalance: Double = 0.101,
  placeOrder: Double = 0.33,
  cancelOrder: Double = 0.33
)

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
