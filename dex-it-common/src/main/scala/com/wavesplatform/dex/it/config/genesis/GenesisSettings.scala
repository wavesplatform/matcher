package com.wavesplatform.dex.it.config.genesis

import com.wavesplatform.dex.domain.bytes.ByteStr

import scala.concurrent.duration.FiniteDuration

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)
