package com.wavesplatform.dex.grpc.integration.clients.domain

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

package object portfolio {
  type AddressAssets = Map[Address, Map[Asset, Long]]
}
