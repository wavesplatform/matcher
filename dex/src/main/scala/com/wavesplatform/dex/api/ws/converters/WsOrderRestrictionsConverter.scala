package com.wavesplatform.dex.api.ws.converters

import com.wavesplatform.dex.api.ws.entities.WsOrderBookRestrictions
import com.wavesplatform.dex.settings.OrderRestrictionsSettings

object WsOrderRestrictionsConverter {

  def toWs(ors: OrderRestrictionsSettings): WsOrderBookRestrictions = WsOrderBookRestrictions(
    stepAmount = ors.stepAmount,
    maxAmount = ors.maxAmount,
    minAmount = ors.minAmount,
    stepPrice = ors.stepPrice,
    maxPrice = ors.maxPrice,
    minPrice = ors.minPrice
  )

}
