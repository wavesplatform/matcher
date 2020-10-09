package com.wavesplatform.dex.api.ws.protocol

import play.api.libs.json._

trait WsClientMessage extends WsMessage

object WsClientMessage {

  implicit val wsClientMessageReads: Reads[WsClientMessage] = Reads { json =>
    (json \ "T").asOpt[String].fold[JsResult[WsClientMessage]](JsError(JsPath, "Type 'T' was not specified")) {
      case WsPingOrPong.tpe => json.validate[WsPingOrPong]
      case WsOrderBookSubscribe.tpe => json.validate[WsOrderBookSubscribe]
      case WsAddressSubscribe.tpe => json.validate[WsAddressSubscribe]
      case WsRatesUpdatesSubscribe.tpe => json.validate[WsRatesUpdatesSubscribe].flatMap(WsRatesUpdatesSubscribe.validateId)
      case WsUnsubscribe.tpe => json.validate[WsUnsubscribe]
      case x => JsError(JsPath \ "T", s"An unknown type: $x")
    }
  }

  val wsClientMessageWrites: Writes[WsClientMessage] = Writes {
    case x: WsPingOrPong => Json.toJson(x)
    case x: WsOrderBookSubscribe => Json.toJson(x)
    case x: WsAddressSubscribe => Json.toJson(x)
    case x: WsRatesUpdatesSubscribe => Json.toJson(x)
    case x: WsUnsubscribe => Json.toJson(x)
    case x => throw new NotImplementedError(x.getClass.getName)
  }

}
