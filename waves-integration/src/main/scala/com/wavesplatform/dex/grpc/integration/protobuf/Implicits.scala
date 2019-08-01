package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.common.primitives.Bytes
import com.wavesplatform.common.utils.EitherExt2
import mouse.any._

object Implicits {

  import com.google.protobuf.{ByteString => PBByteString}
  import com.wavesplatform.account.{AddressScheme, Address => VAddress}
  import com.wavesplatform.protobuf.transaction.{PBAmounts, AssetId => PBAssetId}
  import com.wavesplatform.transaction.{Asset => VAsset}

  implicit class PBByteStringOps(byteString: PBByteString) {
    def toVanillaAddress: VAddress = {
      Bytes.concat(Array(VAddress.AddressVersion, AddressScheme.current.chainId), byteString.toByteArray) |> { header =>
        VAddress.fromBytes { Bytes.concat(header, VAddress calcCheckSum header) } explicitGet ()
      }
    }
  }

  implicit class PBAssetOps(assetId: PBAssetId) {
    def toVanillaAsset: VAsset = PBAmounts.toVanillaAssetId(assetId)
  }

  implicit class VAddressOps(address: VAddress) {
    def toPBAddress: PBByteString = PBByteString.copyFrom(address.bytes.arr.slice(2, address.bytes.arr.length - VAddress.ChecksumLength))
  }

  implicit class VAssetOps(asset: VAsset) {
    def toPBAsset: Option[PBAssetId] = Some(PBAmounts.toPBAssetId(asset))
  }
}
