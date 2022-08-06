package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import monix.eval.Coeval

case class OrderV4(
  orderAuthentication: OrderAuthentication,
  matcherPublicKey: PublicKey,
  assetPair: AssetPair,
  orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long,
  override val feeAsset: Asset
) extends Order {

  override def version: Byte = 4

  override val bodyBytes: Coeval[Array[Byte]] = ???
  override val bytes: Coeval[Array[Byte]] = ???
}
