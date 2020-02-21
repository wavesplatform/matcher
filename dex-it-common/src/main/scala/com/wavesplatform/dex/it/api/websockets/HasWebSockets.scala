package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import mouse.any._

trait HasWebSockets {

  implicit lazy protected val system: ActorSystem        = ActorSystem()
  implicit lazy protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected lazy val knownWsConnections: ConcurrentHashMap.KeySetView[WebSocketConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WebSocketConnection[_]]()

  protected def addConnection(connection: WebSocketConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWebSocketConnection[Output](uri: String,
                                              parseOutput: Message => Output,
                                              trackOutput: Boolean = true): WebSocketConnection[Output] = {
    WebSocketConnection(uri, parseOutput, trackOutput = trackOutput) unsafeTap addConnection
  }

  protected def cleanupWebSockets(): Unit = {
    if (!knownWsConnections.isEmpty) {
      knownWsConnections.forEach(c => if (!c.isClosed) c.close())
      materializer.shutdown()
    }
  }
}
