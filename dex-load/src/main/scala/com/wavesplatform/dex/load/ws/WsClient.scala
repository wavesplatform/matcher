package com.wavesplatform.dex.load.ws

import com.github.andyglow.websocket.{WebsocketClient, _}
import org.slf4j.LoggerFactory

class WsClient(address: String, aus: String, obs: String*) extends WaitForStop[String] {

  val log = LoggerFactory.getLogger(address)

  val protocolHandler = new WebsocketHandler[String]() {
    def receive = {
      case str => {
        if (str.startsWith("""{"T":"pp"""")) {
          log.info(s"PP: $str")
          sender() ! str
        }
        if (str.startsWith("""{"T":"au"""")) {
          log.info(s"AU: $str")
        }
        if (str.startsWith("""{"T":"ob"""")) {
          log.info(s"OB: $str")
        }
      }
    }
  }

  val client = WebsocketClient(stringUri, protocolHandler)

  def run(): Unit = {
    socket ! aus
    socket ! obs(0)
  }
}
