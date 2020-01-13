package com.wavesplatform.dex.it.waves

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.wavesj.matcher.{Order => JOrder}
import com.wavesplatform.wavesj.{ByteString, PrivateKeyAccount, Transactions, AssetPair => JAssetPair}

trait ToWavesJConversions {

  implicit def toSenderJ(sender: KeyPair): PrivateKeyAccount = {
    PrivateKeyAccount.fromPrivateKey(sender.privateKey.base58, AddressScheme.current.chainId)
  }

  implicit def toRecipientJ(recipient: Address): String = recipient.stringRepr
  implicit def toRecipientJ(recipient: KeyPair): String = recipient.toAddress.stringRepr

  implicit def toAssetJ(asset: Asset): String                 = asset.toString
  implicit def toAssetPairJ(assetPair: AssetPair): JAssetPair = new JAssetPair(assetPair.amountAsset, assetPair.amountAsset)

  implicit def toOrderTypeJ(orderType: OrderType): JOrder.Type = if (orderType == OrderType.BUY) JOrder.Type.BUY else JOrder.Type.SELL

  implicit def toByteStr(byteString: ByteString): ByteStr = ByteStr(byteString.getBytes)

  def toOrderJ(orderSender: KeyPair, order: Order): JOrder = {
    Transactions.makeOrder(
      orderSender,
      order.matcherPublicKey.toString,
      order.orderType,
      order.assetPair,
      order.price,
      order.amount,
      order.expiration,
      order.matcherFee,
      order.feeAsset,
      order.timestamp
    )
  }
}

object Implicits extends ToWavesJConversions
