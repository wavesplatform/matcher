package com.wavesplatform.dex.grpc.integration.protobuf

import java.nio.charset.StandardCharsets

import com.google.protobuf.{ByteString => PbByteString}
import com.wavesplatform.dex.domain.account.{Address => VAddress}
import com.wavesplatform.dex.domain.asset.{Asset => VAsset}
import com.wavesplatform.dex.domain.bytes.{ByteStr => VByteStr}
import com.wavesplatform.dex.domain.utils._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.services.AssetDescriptionResponse.MaybeDescription

object PbToDexConversions {

  implicit class PbByteStringOps(val self: PbByteString) extends AnyVal {
    def toVanilla: VByteStr        = VByteStr(self.toByteArray)
    def toVanillaAsset: VAsset     = if (self.isEmpty) VAsset.Waves else VAsset.IssuedAsset(self.toVanilla)
    def toVanillaAddress: VAddress = VAddress.fromBytes(self.toByteArray).explicitGet()
  }

  implicit class PbMaybeDescriptionOps(val self: MaybeDescription) extends AnyVal {
    def toVanilla: Option[BriefAssetDescription] = self match {
      case MaybeDescription.Empty => None
      case MaybeDescription.Description(value) =>
        Some(
          BriefAssetDescription(
            name = value.name.toString(StandardCharsets.UTF_8),
            decimals = value.decimals,
            hasScript = value.hasScript
          )
        )
    }
  }
}
