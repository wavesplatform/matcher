package com.wavesplatform.it

import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.WsFullOrder
import com.wavesplatform.dex.api.ws.protocol.{WsError, WsPingOrPong, WsServerMessage}
import com.wavesplatform.dex.it.api.websockets.HasWebSockets

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.reflect.ClassTag

trait WsSuiteBase extends MatcherSuiteBase with HasWebSockets {

  protected implicit val wsErrorDiff: Diff[WsError] = Derived[Diff[WsError]].ignore[WsError, Long](_.timestamp)

  protected implicit val wsCompleteOrderDiff: Diff[WsFullOrder] =
    Derived[Diff[WsFullOrder]].ignore[WsFullOrder, Long](_.timestamp).ignore[WsFullOrder, Long](_.eventTimestamp)

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

    def receiveAtLeastNPingsOrErrors(n: Int): (List[WsPingOrPong], List[WsError]) = {
      receiveAtLeastN[WsServerMessage](n).foldLeft { (List.empty[WsPingOrPong], List.empty[WsError]) } {
        case ((pings, errors), message) =>
          message match {
            case p: WsPingOrPong => (pings :+ p, errors)
            case e: WsError      => (pings, errors :+ e)
            case _               => (pings, errors)
          }
      }
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
