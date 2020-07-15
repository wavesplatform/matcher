package com.wavesplatform.dex.load

import java.io.File

import akka.actor.ActorSystem
import com.wavesplatform.dex.load.ws.WsCollectChangesClient

import scala.io.Source
import scala.util.Random

object WsAccumulateChanges {

  def createClients(apiUri: String, feederFile: File, accountsNumber: Int)(implicit system: ActorSystem): Seq[WsCollectChangesClient] =
    readRandomAccountLines(feederFile, accountsNumber).map { accountLine =>
      val fields = accountLine.split(';')

      val addr = fields(0)
      val aus  = fields(1)
      val obs  = fields.drop(2).toSeq

      new WsCollectChangesClient(apiUri, addr, aus, obs)
    }

  private def readRandomAccountLines(feederFile: File, accountsNumber: Int): Seq[String] = {
    val source = Source.fromFile(feederFile)
    try {
      val lines = source.getLines()
      val r     = lines.take(accountsNumber).toArray
      lines.foreach { line =>
        // 30%
        if (Random.nextDouble() < 0.3) r.update(Random.nextInt(accountsNumber), line)
      }
      r.toSeq
    } finally source.close()
  }
}
