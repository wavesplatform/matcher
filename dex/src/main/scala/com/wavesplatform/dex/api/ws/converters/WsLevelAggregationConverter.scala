package com.wavesplatform.dex.api.ws.converters

import com.wavesplatform.dex.api.ws.protocol.WsOrderBookChanges.LevelAggregation
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.model.LevelAgg

object WsLevelAggregationConverter {

  def toWs(side: OrderType)(levelAgg: LevelAgg): LevelAggregation = LevelAggregation(levelAgg.price, levelAgg.amount, side)

}
