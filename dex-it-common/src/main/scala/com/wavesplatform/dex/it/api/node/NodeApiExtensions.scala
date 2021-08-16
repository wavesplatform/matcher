package com.wavesplatform.dex.it.api.node

import cats.Id
import com.wavesplatform.transactions.Transaction

trait NodeApiExtensions {

  this: HasWavesNode =>

  protected def broadcast(tx: Transaction): Unit = wavesNode1.api broadcast tx

  protected def broadcastAndAwait(txs: Transaction*): Unit = broadcastAndAwait(wavesNode1.api, txs: _*)

  protected def broadcastAndAwait(wavesNodeApi: NodeApi[Id], txs: Transaction*): Unit = {
    txs.foreach(wavesNodeApi.broadcast)
    txs.foreach(wavesNodeApi.waitForTransaction)
  }

}
