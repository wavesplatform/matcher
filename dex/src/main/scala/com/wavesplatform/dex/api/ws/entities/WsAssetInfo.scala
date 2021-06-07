package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.domain.asset.Asset

case class WsAssetInfo(asset: Asset, balances: WsBalances, isNft: Boolean)
