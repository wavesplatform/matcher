package com.wavesplatform.dex.grpc.integration.clients.combined

import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.meta.getSimpleName
import monix.reactive.Observable
import play.api.libs.json.{Format, JsError, JsNumber, JsObject, JsString, JsSuccess, Reads, Writes}

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

  sealed abstract class Status extends Product with Serializable

  object Status {

    final case class Starting(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Stopping(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Closing(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends Status with HasStreams
    final case class Working(height: Int) extends Status

    implicit val format: Format[Status] = Format(
      Reads { json =>
        (json \ "status").as[String] match {
          case "Starting" => JsSuccess(Starting())
          case "Stopping" => JsSuccess(Stopping())
          case "Closing" => JsSuccess(Closing())
          case "Working" =>
            val height = (json \ "height").as[Int]
            JsSuccess(Working(height))
          case x => JsError(s"Can't parse '$x' as CombinedStream.Status")
        }
      },
      Writes {
        case _: Starting => JsObject(Map("status" -> JsString("Starting")))
        case _: Stopping => JsObject(Map("status" -> JsString("Stopping")))
        case _: Closing => JsObject(Map("status" -> JsString("Closing")))
        case x: Working => JsObject(Map("status" -> JsString("Working"), "height" -> JsNumber(x.height)))
      }
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
