package com.wavesplatform.dex.grpc.integration.dto

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset

case class BalanceChangesResponse(address: Address, asset: Asset, balance: Long)
