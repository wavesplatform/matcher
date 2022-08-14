package com.wavesplatform.dex.domain.order

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs

final class OrderOps(val o: Order) extends AnyVal {

  @inline def copy(withV1: OrderV1 => OrderV1, withV2: OrderV2 => OrderV2, withV3: OrderV3 => OrderV3, withV4: OrderV4 => OrderV4): Order =
    o match {
      case o1: OrderV1 => withV1(o1)
      case o2: OrderV2 => withV2(o2)
      case o3: OrderV3 => withV3(o3)
      case o4: OrderV4 => withV4(o4)
      case _ => throw new IllegalArgumentException(s"Can't process the order ${o.id()} of version ${o.version}")
    }

  @inline def updateProofs(p: Proofs): Order =
    copy(
      x => x.copy(orderAuthentication = x.orderAuthentication.updateProofs(p)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateProofs(p)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateProofs(p)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateProofs(p))
    )

  @inline def updateEip712Signature(sig: ByteStr): Order =
    copy(
      x => x.copy(orderAuthentication = x.orderAuthentication.updateEip712Signature(sig)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateEip712Signature(sig)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateEip712Signature(sig)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateEip712Signature(sig))
    )

  @inline def updateExpiration(expiration: Long): Order =
    copy(
      _.copy(expiration = expiration),
      _.copy(expiration = expiration),
      _.copy(expiration = expiration),
      _.copy(expiration = expiration)
    )

  @inline def updateTimestamp(timestamp: Long): Order =
    copy(
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp)
    )

  @inline def updateFee(fee: Long): Order =
    copy(
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee)
    )

  @inline def updateAmount(amount: Long): Order =
    copy(
      _.copy(amount = amount),
      _.copy(amount = amount),
      _.copy(amount = amount),
      _.copy(amount = amount)
    )

  @inline def updatePrice(price: Long): Order =
    copy(
      _.copy(price = price),
      _.copy(price = price),
      _.copy(price = price),
      _.copy(price = price)
    )

  @inline def updateMatcher(pk: KeyPair): Order =
    copy(
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk)
    )

  @inline def updateSender(pk: KeyPair): Order =
    copy(
      x => x.copy(orderAuthentication = x.orderAuthentication.updateSender(pk)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateSender(pk)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateSender(pk)),
      x => x.copy(orderAuthentication = x.orderAuthentication.updateSender(pk))
    )

  @inline def updatePair(pair: AssetPair): Order =
    copy(
      _.copy(assetPair = pair),
      _.copy(assetPair = pair),
      _.copy(assetPair = pair),
      _.copy(assetPair = pair)
    )

  @inline def updateType(t: OrderType): Order =
    copy(
      _.copy(orderType = t),
      _.copy(orderType = t),
      _.copy(orderType = t),
      _.copy(orderType = t)
    )

  @inline def updateFeeAsset(a: Asset): Order =
    copy(
      identity,
      identity,
      _.copy(feeAsset = a),
      _.copy(feeAsset = a)
    )

}

object OrderOps {
  implicit def toOps(o: Order): OrderOps = new OrderOps(o)
}
