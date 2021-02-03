package com.wavesplatform.it

import cats.syntax.either._
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.connection.WsConnection.WsRawMessage
import com.wavesplatform.dex.api.ws.entities.WsFullOrder
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsError, WsOrderBookChanges, WsPingOrPong, WsServerMessage}
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.reflect.ClassTag

trait WsSuiteBase extends MatcherSuiteBase with HasWebSockets with DiffMatcherWithImplicits {

  implicit protected val wsErrorDiff: Derived[Diff[WsError]] = Derived(Diff.gen[WsError].value.ignore[WsError, Long](_.timestamp))

  implicit protected val wsAddressChangesDiff: Derived[Diff[WsAddressChanges]] = Derived(
    Diff.gen[WsAddressChanges].value
      .ignore[WsAddressChanges, Long](_.timestamp)
      .ignore[WsAddressChanges, Long](_.updateId)
  )

  implicit private val wsOrderBookChangesDiff: Derived[Diff[WsOrderBookChanges]] =
    Derived(Diff.gen[WsOrderBookChanges].value.ignore[WsOrderBookChanges, Long](_.timestamp).ignore[WsOrderBookChanges, Long](_.updateId))

  implicit protected val wsCompleteOrderDiff: Diff[WsFullOrder] =
    Derived(Diff.gen[WsFullOrder].value.ignore[WsFullOrder, Long](_.timestamp).ignore[WsFullOrder, Long](_.eventTimestamp))

  implicit final class WsConnectionOps(val self: WsConnection) {

    def receiveAtLeastN[T <: WsServerMessage: ClassTag](n: Int): List[T] = {
      val r = eventually {
        val xs = self.collectMessages[T]
        xs.size should be >= n
        xs
      }
      Thread.sleep(200) // Waiting for additional messages
      r
    }

    def receiveAtLeastNRaw(n: Int): List[WsRawMessage] = {
      val r = eventually {
        val xs = self.rawMessages
        xs.size should be >= n
        xs
      }
      Thread.sleep(200) // Waiting for additional messages
      r
    }

    def receiveAtLeastNErrorsAndPings(errorsNumber: Int, pingsNumber: Int): (List[WsError], List[WsPingOrPong]) =
      eventually {
        val (errors, pings) = self
          .collectMessages[WsServerMessage]
          .filter {
            case _: WsPingOrPong => true
            case _: WsError => true
            case _ => false
          }
          .partitionMap {
            case x: WsError => x.asLeft
            case x: WsPingOrPong => x.asRight
            case _ => throw new IllegalArgumentException(s"Unexpected error")
          }

        errors.size should be >= errorsNumber
        pings.size should be >= pingsNumber
        (errors, pings)
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
