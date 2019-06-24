package com.wavesplatform.dex.api
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
