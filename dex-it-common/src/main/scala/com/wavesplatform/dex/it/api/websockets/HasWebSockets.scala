package com.wavesplatform.dex.it.api.websockets

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer

trait HasWebSockets {

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected def mkWebSocketConnection[Output](uri: String,
                                              parseOutput: Message => Output,
                                              trackOutput: Boolean = true): WebSocketConnection[Output] = {
    WebSocketConnection(uri, parseOutput, trackOutput = trackOutput)
  }
}
