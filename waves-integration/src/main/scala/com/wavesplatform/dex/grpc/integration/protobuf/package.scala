package com.wavesplatform.dex.grpc.integration

package object protobuf {

  type PBSBalanceChangesResponse = com.wavesplatform.dex.grpc.integration.services.BalanceChangesResponse
  val PBSBalanceChangesResponse = com.wavesplatform.dex.grpc.integration.services.BalanceChangesResponse

  type VanillaBalanceChangesResponse = com.wavesplatform.dex.grpc.integration.dto.BalanceChangesResponse
}
