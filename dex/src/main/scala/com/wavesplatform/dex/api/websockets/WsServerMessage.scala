package com.wavesplatform.dex.api.websockets

import play.api.libs.json.{JsError, JsPath, JsResult, Reads}

trait WsServerMessage extends WsMessage

object WsServerMessage {
  implicit val wsServerMessageReads: Reads[WsServerMessage] = Reads { json =>
    (json \ "T").asOpt[String].fold[JsResult[WsServerMessage]](JsError(JsPath, "Type 'T' was not specified")) {
      case WsPingOrPong.tpe   => json.validate[WsPingOrPong]
      case WsAddressState.tpe => json.validate[WsAddressState]
      case WsOrderBook.tpe    => json.validate[WsOrderBook]
      case x                  => JsError(JsPath \ "T", s"An unknown type: $x")
    }
  }
}
