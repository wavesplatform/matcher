package com.wavesplatform.dex.it.api.node

import cats.Id
import com.wavesplatform.transaction.Transaction

trait NewWavesNodeApiExtensions {

  this: NewHasWavesNode =>

  protected def broadcastAndAwait(txs: Transaction*): Unit = broadcastAndAwait(wavesNodeApi, txs: _*)

  protected def broadcastAndAwait(wavesNodeApi: NodeApi[Id], txs: Transaction*): Unit = {
    txs map wavesNodeApi.broadcast
    txs foreach wavesNodeApi.waitForTransaction
  }
}
