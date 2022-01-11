package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.OrderType

final case class HttpLastTrade(price: Long, amount: Long, side: OrderType) {
  override def toString: String = s"LastTrade(p=$price, a=$amount, $side)"
}
