package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.Asset

case class WsAddressState(balances: Map[Asset, WsBalances])
