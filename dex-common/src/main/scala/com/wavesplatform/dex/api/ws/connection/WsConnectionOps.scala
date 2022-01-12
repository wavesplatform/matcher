package com.wavesplatform.dex.api.ws.connection

import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsPingOrPong, WsServerMessage}
import com.wavesplatform.dex.domain.asset.Asset

import scala.reflect.ClassTag

trait WsConnectionOps {

  implicit final class Ops(self: WsConnection) {
    def collectMessages[T <: WsServerMessage: ClassTag]: List[T] = self.messages.collect { case x: T => x }
    def pings: List[WsPingOrPong] = collectMessages[WsPingOrPong]
    def addressStateChanges: List[WsAddressChanges] = collectMessages[WsAddressChanges]
    def balanceChanges: List[Map[Asset, WsBalances]] = addressStateChanges.map(_.balances).filter(_.nonEmpty)
    def orderChanges: List[WsOrder] = addressStateChanges.flatMap(_.orders)
  }

}

object WsConnectionOps extends WsConnectionOps
