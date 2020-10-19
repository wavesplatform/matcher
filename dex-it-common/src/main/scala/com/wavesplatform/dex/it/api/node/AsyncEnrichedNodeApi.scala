package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress

import com.softwaremill.sttp._
import com.softwaremill.sttp.playJson._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.it.api.responses.node._
import com.wavesplatform.dex.it.api.{AsyncEnrichedApi, EnrichedResponse}
import im.mak.waves.transactions.Transaction
import im.mak.waves.transactions.common.Id

import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedNodeApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends AsyncEnrichedApi[ErrorResponse](host)
    with NodeApi[AsyncEnrichedNodeApi.R] {

  override def wavesBalanceOrig(address: Address): R[WavesBalanceResponse] = mk {
    sttp.get(uri"$apiUri/addresses/balance/$address")
  }

  override def assetBalanceOrig(address: Address, asset: Asset.IssuedAsset): R[AssetBalanceResponse] = mk {
    sttp.get(uri"$apiUri/assets/balance/$address/$asset")
  }

  override def broadcast(tx: Transaction): R[Unit] = mkIgnore {
    sttp.post(uri"$apiUri/transactions/broadcast").body(tx.toJson).contentType("application/json")
  }

  override def transactionInfo(id: Id): R[Transaction] = mk {
    sttp.get(uri"$apiUri/transactions/info/$id")
  }

  override def currentHeightOrig: R[HeightResponse] = mk {
    sttp.get(uri"$apiUri/blocks/height")
  }

  override def activationStatus: R[ActivationStatusResponse] = mk {
    sttp.get(uri"$apiUri/activation/status")
  }

  override def connect(toNode: InetSocketAddress): R[Unit] = mkIgnore {
    sttp
      .post(uri"$apiUri/peers/connect")
      .body(ConnectReq(toNode.getHostName, toNode.getPort))
      .header("X-API-Key", apiKey)
  }

  override def connectedPeers: R[ConnectedPeersResponse] = mk {
    sttp.get(uri"$apiUri/peers/connected")
  }

}

object AsyncEnrichedNodeApi {
  type R[EntityT] = Future[EnrichedResponse[ErrorResponse, EntityT]]
}
