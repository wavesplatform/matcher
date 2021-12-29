package com.wavesplatform.dex.api.ws.entities

final case class WsOrderBookRestrictions(
  stepAmount: Double = 0.00000001,
  minAmount: Double = 0.00000001,
  maxAmount: Double = 1000000000,
  stepPrice: Double = 0.00000001,
  minPrice: Double = 0.00000001,
  maxPrice: Double = 1000000
)
