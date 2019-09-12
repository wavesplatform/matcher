package com.wavesplatform.dex.it.test

import com.wavesplatform.transaction.Transaction

trait WavesNodeApiExtensions {
  this: HasWavesNode =>

  protected def broadcastAndAwait(txs: Transaction*): Unit = {
    txs.map(wavesNode1Api.broadcast)
    txs.foreach(tx => wavesNode1Api.waitForTransaction(tx))
  }
}
