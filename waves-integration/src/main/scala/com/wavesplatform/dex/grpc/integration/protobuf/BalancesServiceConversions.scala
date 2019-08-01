package com.wavesplatform.dex.grpc.integration.protobuf

import com.wavesplatform.dex.grpc.integration.dto.BalanceChangesResponse
import com.wavesplatform.dex.grpc.integration.protobuf.Implicits._
import mouse.any._

object BalancesServiceConversions {

  def vanilla(balanceChangesResponse: PBSBalanceChangesResponse): VanillaBalanceChangesResponse = {
    BalanceChangesResponse(
      balanceChangesResponse.address.toVanillaAddress,
      balanceChangesResponse.getAsset.toVanillaAsset,
      balanceChangesResponse.balance
    )
  }

  def protobuf(balanceChangesResponse: VanillaBalanceChangesResponse): PBSBalanceChangesResponse = balanceChangesResponse |> {
    case BalanceChangesResponse(address, asset, balance) => PBSBalanceChangesResponse(address.toPBAddress, asset.toPBAsset, balance)
  }
}
