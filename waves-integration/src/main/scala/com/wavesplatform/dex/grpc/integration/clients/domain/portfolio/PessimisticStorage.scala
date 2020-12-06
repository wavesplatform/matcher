package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

import scala.collection.mutable

private[portfolio] class PessimisticStorage {
  // Longs are negative in both maps, see getPessimisticPortfolio
  val portfolios = new mutable.AnyRefMap[Address, Map[Asset, Long]]()
  val txs = new mutable.AnyRefMap[ByteString, Map[Address, Map[Asset, Long]]]
}
