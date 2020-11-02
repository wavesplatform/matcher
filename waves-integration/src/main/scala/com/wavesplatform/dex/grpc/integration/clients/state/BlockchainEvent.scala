package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.collection.MapOps.Ops
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

sealed trait BlockchainEvent extends Product with Serializable

object BlockchainEvent {
  case class Append(changes: BlockchainData) extends BlockchainEvent

  case class AppendMicro(changes: BlockchainData) extends BlockchainEvent

  case class Rollback(commonBlockInfo: BlockInfo) extends BlockchainEvent

  // Happens either on a fork or during an appending of a key block
  case class RollbackMicro(commonBlockInfo: BlockInfo) extends BlockchainEvent

  case class DataUpdate(regularBalances: Map[Address, Map[Asset, Long]], outLeases: Map[Address, Long]) extends BlockchainEvent {

    def combine(other: DataUpdate): DataUpdate = DataUpdate(
      regularBalances = regularBalances.deepReplace(other.regularBalances),
      outLeases = outLeases ++ other.outLeases
    )

  }

}
