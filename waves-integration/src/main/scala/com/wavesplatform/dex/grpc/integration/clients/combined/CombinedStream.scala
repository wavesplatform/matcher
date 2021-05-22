package com.wavesplatform.dex.grpc.integration.clients.combined

import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.meta.getSimpleName
import monix.reactive.Observable
import play.api.libs.json.{Format, Reads, Writes}

import scala.concurrent.duration.FiniteDuration

trait CombinedStream {
  def startFrom(height: Int): Unit
  def restart(): Unit

  def currentStatus: Status

  def updateProcessedHeight(height: Int): Unit
  def currentProcessedHeight: Int

  val stream: Observable[WavesNodeEvent]
}

object CombinedStream {

  sealed abstract class Status extends Product with Serializable {
    val name: String = getSimpleName(this)
  }

  object Status {

    final case class Starting(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Stopping(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Closing(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case object Working extends Status

    val All = List(Starting(), Stopping(), Closing(), Working)

    implicit val format: Format[Status] = Format(
      Reads.StringReads.map { x =>
        All.find(_.name == x) match {
          case Some(r) => r
          case None => throw new IllegalArgumentException(s"Can't parse '$x' as CombinedStream.Status")
        }
      },
      Writes.StringWrites.contramap(_.name)
    )

    sealed trait HasStreams {
      def blockchainUpdates: Boolean
      def utxEvents: Boolean

      def oneDone: Boolean = blockchainUpdates || utxEvents
      override def toString: String = s"${getSimpleName(this)}(b=$blockchainUpdates, u=$utxEvents)"
    }

  }

  case class Settings(restartDelay: FiniteDuration)

}
