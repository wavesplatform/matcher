package com.wavesplatform.dex.it.waves

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.wavesj.{ByteString, PrivateKeyAccount}

object Implicits {

  implicit def toSenderJ(sender: KeyPair): PrivateKeyAccount = {
    PrivateKeyAccount.fromPrivateKey(sender.privateKey.base58, AddressScheme.current.chainId)
  }

  implicit def toRecipientJ(recipient: Address): String = recipient.stringRepr

  implicit def toAssetJ(asset: Asset): String = asset.toString

  implicit def toByteStr(byteString: ByteString): ByteStr = ByteStr(byteString.getBytes)

}
