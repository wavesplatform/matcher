package com.wavesplatform.dex.it.api.responses.node

import com.wavesplatform.dex.it.api.responses.node.ConnectedPeersResponse.PeerInfo
import play.api.libs.json.{Format, Json}

case class ConnectedPeersResponse(peers: List[PeerInfo])
object ConnectedPeersResponse {
  implicit val format: Format[ConnectedPeersResponse] = Json.format[ConnectedPeersResponse]

  case class PeerInfo(address: String, declaredAddress: String, peerName: String, peerNonce: Long, applicationName: String, applicationVersion: String)
  object PeerInfo {
    implicit val format: Format[PeerInfo] = Json.format[PeerInfo]
  }
}
