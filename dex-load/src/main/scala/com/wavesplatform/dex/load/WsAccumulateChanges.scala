package com.wavesplatform.dex.load

import java.io.File
import akka.actor.ActorSystem
import com.wavesplatform.dex.load.WavesDexLoadCli.WsCheckType
import com.wavesplatform.dex.load.WavesDexLoadCli.WsCheckType.CheckLeaps
import com.wavesplatform.dex.load.ws.WsCollectChangesClient

import scala.io.Source
import scala.util.{Random, Using}

object WsAccumulateChanges {

  def createClients(apiUri: String, feederFile: File, accountsNumber: Int, wsCheckType: WsCheckType)(implicit
    system: ActorSystem
  ): Seq[WsCollectChangesClient] = {
    val accountLines =
      if (wsCheckType == CheckLeaps) readLastAccountLines(feederFile, accountsNumber) else readRandomAccountLines(feederFile, accountsNumber)

    accountLines.map { accountLine =>
      val fields = accountLine.split(';')

      val addr = fields(0)
      val aus = fields(1)
      val obs = fields.drop(2).toSeq

      new WsCollectChangesClient(apiUri, addr, aus, obs)
    }
  }

  private def readLastAccountLines(feederFile: File, accountsNumber: Int): Seq[String] =
    Using.resource(Source.fromFile(feederFile)) { source =>
      source.getLines().toList.takeRight(accountsNumber)
    }

  private def readRandomAccountLines(feederFile: File, accountsNumber: Int): Seq[String] =
    Using.resource(Source.fromFile(feederFile)) { source =>
      val lines = source.getLines()
      val r = lines.take(accountsNumber).toArray
      lines.foreach { line =>
        // 30%
        if (Random.nextDouble() < 0.3) r.update(Random.nextInt(accountsNumber), line)
      }
      r.toSeq
    }

}
