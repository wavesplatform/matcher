package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.it.docker.DexContainer
import mouse.any._
import org.scalatest.{BeforeAndAfterAll, Suite}

trait HasWebSockets extends BeforeAndAfterAll { _: Suite =>

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WebSocketConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WebSocketConnection[_]]()

  protected def addConnection(connection: WebSocketConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWebSocketConnection[Output](uri: String,
                                              parseOutput: Message => Output,
                                              trackOutput: Boolean = true): WebSocketConnection[Output] = {
    new WebSocketConnection(uri, parseOutput, trackOutput = trackOutput) unsafeTap addConnection
  }

  protected def mkWebSocketAuthenticatedConnection(client: KeyPair, dex: DexContainer): WebSocketAuthenticatedConnection = {

    val prefix        = "as"
    val timestamp     = System.currentTimeMillis()
    val signedMessage = prefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/accountUpdates/${client.publicKey}?Timestamp=$timestamp&Signature=$signature"

    new WebSocketAuthenticatedConnection(wsUri)
  }

  protected def mkWebSocketConnection[Output](uri: String)(parseOutput: Message => Output): WebSocketConnection[Output] = {
    WebSocketConnection(uri, parseOutput, trackOutput = true) unsafeTap addConnection
  }

  protected def cleanupWebSockets(): Unit = {
    if (!knownWsConnections.isEmpty) {
      knownWsConnections.forEach(c => if (!c.isClosed) c.close())
      materializer.shutdown()
    }
  }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
  }
}
