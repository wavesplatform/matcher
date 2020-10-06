package com.wavesplatform.dex.it.api.node

import cats.Id
import im.mak.waves.transactions.Transaction

trait NodeApiExtensions {

  this: HasWavesNode =>

  protected def broadcastAndAwait(txs: Transaction*): Unit = broadcastAndAwait(wavesNode1.api, txs: _*)

  protected def broadcastAndAwait(wavesNodeApi: NodeApi[Id], txs: Transaction*): Unit = {
    txs.foreach(wavesNodeApi.broadcast)
    txs.foreach(wavesNodeApi.waitForTransaction)
  }

}
