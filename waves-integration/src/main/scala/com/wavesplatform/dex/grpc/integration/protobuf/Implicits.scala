package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBAmounts
import com.wavesplatform.transaction.Asset
import mouse.any._

object Implicits {

  implicit class ByteStringOps(byteString: ByteString) {
    def toVanillaAddress: Address = {
      Bytes.concat(Array(Address.AddressVersion, AddressScheme.current.chainId), byteString.toByteArray) |> { header =>
        Address.fromBytes { Bytes.concat(header, Address calcCheckSum header) } explicitGet ()
      }
    }
    def toVanillaAsset: Asset = PBAmounts.toVanillaAssetId(byteString)
  }

  implicit class AddressOps(address: Address) {
    def toPBAddress: ByteString = ByteString.copyFrom(address.bytes.arr.slice(2, address.bytes.arr.length - Address.ChecksumLength))
  }

  implicit class AssetOps(asset: Asset) {
    def toPBAsset: ByteString = PBAmounts.toPBAssetId(asset)
  }
}
