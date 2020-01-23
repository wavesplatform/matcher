package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
