package com.wavesplatform.dex.api.http.routes

import akka.serialization.SerializerWithStringManifest
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.json.{Format, Json, Reads, Writes}
import com.wavesplatform.dex.domain.order.OrderJson._

trait WithMySerialization

class MySerialization extends SerializerWithStringManifest with ScorexLogging {
  import MySerialization._

  val Envelope      = "Envelope"
  val OrderAccepted = "OrderAccepted"
  val OrderCanceled = "OrderCanceled"

  override def identifier: Int = 999

  override def manifest(o: AnyRef): String = o match {
    case _: AddressDirectoryActor.Envelope   => Envelope
    case _: AddressActor.Event.OrderAccepted => OrderAccepted
    case _: AddressActor.Event.OrderCanceled => OrderCanceled
  }

  override def toBinary(o: AnyRef): Array[Byte] = {
    Json.toBytes(o match {
      case x: AddressDirectoryActor.Envelope   => Json.toJson(x)
      case x: AddressActor.Event.OrderAccepted => Json.toJson(x)
      case x: AddressActor.Event.OrderCanceled => Json.toJson(x)
    })
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = {
    val json = Json.parse(bytes)
    manifest match {
      case Envelope      => json.as[AddressDirectoryActor.Envelope]
      case OrderAccepted => json.as[AddressActor.Event.OrderAccepted]
      case OrderCanceled => json.as[AddressActor.Event.OrderCanceled]
    }
  }
}

object MySerialization {
  implicit val orderAcceptedFormat: Format[AddressActor.Event.OrderAccepted] = Json.format[AddressActor.Event.OrderAccepted]
  implicit val orderCanceledFormat: Format[AddressActor.Event.OrderCanceled] = Json.format[AddressActor.Event.OrderCanceled]

  implicit val sourceReads: Reads[Source] = implicitly[Reads[String]].map {
    case "NotTracked"      => Source.NotTracked
    case "Request"         => Source.Request
    case "Expiration"      => Source.Expiration
    case "BalanceTracking" => Source.BalanceTracking
  }

  implicit val sourceWrites: Writes[Source] = implicitly[Writes[String]].contramap {
    case Source.NotTracked      => "NotTracked"
    case Source.Request         => "Request"
    case Source.Expiration      => "Expiration"
    case Source.BalanceTracking => "BalanceTracking"
  }

  implicit val placeOrderFormat: Format[AddressActor.Command.PlaceOrder]   = Json.format[AddressActor.Command.PlaceOrder]
  implicit val cancelOrderFormat: Format[AddressActor.Command.CancelOrder] = Json.format[AddressActor.Command.CancelOrder]

  implicit val messageWrites: Writes[AddressActor.Message] = Writes {
    case x: AddressActor.Command.PlaceOrder =>
      Json.obj(
        "type"  -> "PlaceOrder",
        "inner" -> placeOrderFormat.writes(x)
      )

    case x: AddressActor.Command.CancelOrder =>
      Json.obj(
        "type"  -> "CancelOrder",
        "inner" -> cancelOrderFormat.writes(x)
      )
  }

  implicit val messageReads: Reads[AddressActor.Message] = Reads { json =>
    (json \ "type").as[String] match {
      case "PlaceOrder"  => (json \ "inner").validate[AddressActor.Command.PlaceOrder]
      case "CancelOrder" => (json \ "inner").validate[AddressActor.Command.CancelOrder]
    }
  }

  implicit val envelopeFormat: Format[AddressDirectoryActor.Envelope] = Json.format[AddressDirectoryActor.Envelope]
}
