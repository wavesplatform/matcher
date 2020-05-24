package com.wavesplatform.dex.load

import java.io.File

import com.wavesplatform.dex.load.ws.WsApiClient

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object GatlingChecker {

  def check(apiUri: String, feederFile: File, stop: Future[Unit]): Unit = {
    val source  = Source.fromFile(feederFile)
    var clients = List.empty[WsApiClient]
    try {
      source.getLines().foreach { addressLine =>
        val fields = addressLine.split(';')

        val addr = fields(0)
        val aus  = fields(1)
        val obs  = fields.drop(2)

        val newClient = new WsApiClient(apiUri, addr, aus, obs)
        clients = newClient :: clients
        newClient.run()
        Await.result(stop, Duration.Inf)
      }
    } finally {
      source.close()
      clients.foreach(_.close())
    }
  }
}
