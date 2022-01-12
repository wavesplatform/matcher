package com.wavesplatform.dex.statuses

import com.wavesplatform.dex.meta.getSimpleName
import play.api.libs.json.{Format, JsError, JsNumber, JsObject, JsString, JsSuccess, Reads, Writes}

sealed abstract class CombinedStreamStatus extends Product with Serializable

object CombinedStreamStatus {

  sealed trait HasStreams {
    def blockchainUpdates: Boolean
    def utxEvents: Boolean

    def oneDone: Boolean = blockchainUpdates || utxEvents
    override def toString: String = s"${getSimpleName(this)}(b=$blockchainUpdates, u=$utxEvents)"
  }

  final case class Starting(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends CombinedStreamStatus with HasStreams

  final case class Stopping(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends CombinedStreamStatus with HasStreams

  final case class Closing(blockchainUpdates: Boolean = false, utxEvents: Boolean = false) extends CombinedStreamStatus with HasStreams

  final case class Working(height: Int) extends CombinedStreamStatus

  implicit val format: Format[CombinedStreamStatus] = Format(
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

}
