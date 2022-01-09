package com.wavesplatform.dex.api.ws.converters

import com.wavesplatform.dex.api.ws.protocol.WsOrderBookChanges.LTrade
import com.wavesplatform.dex.model.LastTrade

object WsLastTradeConverter {

  def toWs(lt: LastTrade): LTrade = LTrade(lt.price, lt.amount, lt.side)

}
