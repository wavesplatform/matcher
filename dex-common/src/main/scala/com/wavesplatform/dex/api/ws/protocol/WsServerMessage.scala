package com.wavesplatform.dex.api.ws.protocol

import play.api.libs.json._

trait WsServerMessage extends WsMessage {
  val timestamp: Long
}

object WsServerMessage {

  // Will be never propagated to a client
  case object Complete extends WsServerMessage {
    override val tpe: String = "unused"
    override val timestamp: Long = 0L
  }

  // TODO play-json discriminator
  implicit val wsServerMessageReads: Reads[WsServerMessage] = Reads { json =>
    (json \ "T").asOpt[String].fold[JsResult[WsServerMessage]](JsError(JsPath, "Type 'T' was not specified")) {
      case WsPingOrPong.tpe => json.validate[WsPingOrPong]
      case WsAddressChanges.tpe => json.validate[WsAddressChanges]
      case WsOrderBookChanges.tpe => json.validate[WsOrderBookChanges]
      case WsOrdersUpdate.tpe => json.validate[WsOrdersUpdate]
      case WsError.tpe => json.validate[WsError]
      case WsInitial.tpe => json.validate[WsInitial]
      case WsRatesUpdates.tpe => json.validate[WsRatesUpdates]
      case x => JsError(JsPath \ "T", s"An unknown type: $x")
    }
  }

  val wsServerMessageWrites: Writes[WsServerMessage] = Writes {
    case x: WsPingOrPong => Json.toJson(x)
    case x: WsAddressChanges => Json.toJson(x)
    case x: WsOrderBookChanges => Json.toJson(x)
    case x: WsOrdersUpdate => Json.toJson(x)
    case x: WsError => Json.toJson(x)
    case x: WsInitial => Json.toJson(x)
    case x: WsRatesUpdates => Json.toJson(x)
    case x => throw new NotImplementedError(x.getClass.getName)
  }

}
