package com.wavesplatform.dex.load

import com.wavesplatform.dex.load.ws.WsClient

import scala.io.Source
import scala.util.Random


object GatlingChecker {

  def check(f: String): Unit = {
    val l = Random.shuffle(Source.fromFile(f).getLines.toList)

    for(i <-  0 to 10) {
      val data =  l(i).split(";") //TODO: shit-code only for example usage

      val addr = data(0)
      val aus = data(1)
      val obs1 = data(2)
      val obs2 = data(3)

      new WsClient(addr, aus, obs1, obs2).run()
    }
  }
}
