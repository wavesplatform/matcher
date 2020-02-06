package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.Asset

case class Balances(tradable: Long, reserved: Long)
case class WsBalance(balances: Map[Asset, Balances])
