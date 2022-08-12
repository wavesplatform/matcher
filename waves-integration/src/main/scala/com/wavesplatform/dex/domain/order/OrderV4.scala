package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
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

  override val version: Byte = 4

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(PBUtils.encodeDeterministic(copy(orderAuthentication = orderAuthentication.withoutProofs()).toPB))

  override val bytes: Coeval[Array[Byte]] = ???
}
