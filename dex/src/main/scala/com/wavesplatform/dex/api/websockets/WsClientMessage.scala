package com.wavesplatform.dex.api.websockets

import play.api.libs.json.{JsError, JsPath, JsResult, Reads}

trait WsClientMessage extends WsMessage

object WsClientMessage {
  val wsClientMessageReads: Reads[WsClientMessage] = Reads { json =>
    (json \ "T").asOpt[String].fold[JsResult[WsClientMessage]](JsError(JsPath, "Type 'T' was not specified")) {
      case WsPingOrPong.tpe         => json.validate[WsPingOrPong]
      case WsOrderBookSubscribe.tpe => json.validate[WsOrderBookSubscribe]
      case WsAddressSubscribe.tpe   => json.validate[WsAddressSubscribe]
      case x                        => JsError(JsPath \ "T", s"An unknown type: $x")
    }
  }
}
