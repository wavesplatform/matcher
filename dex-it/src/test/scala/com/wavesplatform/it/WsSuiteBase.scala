package com.wavesplatform.it

import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol.{WsError, WsServerMessage}
import com.wavesplatform.dex.it.api.websockets.HasWebSockets

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.reflect.ClassTag

trait WsSuiteBase extends MatcherSuiteBase with HasWebSockets {

  protected implicit val wsErrorDiff: Diff[WsError] = Derived[Diff[WsError]].ignore[WsError, Long](_.timestamp)

  final implicit class WsConnectionOps(val self: WsConnection) {
    def receiveAtLeastN[T <: WsServerMessage: ClassTag](n: Int): List[T] = {
      val r = eventually {
        val xs = self.collectMessages[T]
        xs.size should be >= n
        xs
      }
      Thread.sleep(200) // Waiting for additional messages
      r
    }

    def receiveNoMessages(duration: FiniteDuration = 1.second): Unit = {
      val sizeBefore = self.messages.size
      Thread.sleep(duration.toMillis)
      self.messages.size shouldBe sizeBefore
    }

    def receiveNoMessagesOf[T <: WsServerMessage: ClassTag](duration: FiniteDuration = 1.second): Unit = {
      val sizeBefore = self.collectMessages[T].size
      Thread.sleep(duration.toMillis)
      self.collectMessages[T].size shouldBe sizeBefore
    }
  }
}
