package com.wavesplatform.dex.it.api.node

import com.wavesplatform.dex.api.http.entities.HttpMessage
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.it.api.responses.node._
import com.wavesplatform.dex.it.api.{AsyncEnrichedApi, EnrichedResponse}
import im.mak.waves.transactions.Transaction
import im.mak.waves.transactions.common.Id
import play.api.libs.json._
import sttp.client3._
import sttp.model._
import sttp.client3.playJson._

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedNodeApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Any])
    extends AsyncEnrichedApi[ErrorResponse](host)
    with NodeApi[AsyncEnrichedNodeApi.R] {

  override def wavesBalanceOrig(address: Address): R[WavesBalanceResponse] = mk {
    basicRequest.get(uri"$apiUri/addresses/balance/$address")
  }

  override def assetBalanceOrig(address: Address, asset: Asset.IssuedAsset): R[AssetBalanceResponse] = mk {
    basicRequest.get(uri"$apiUri/assets/balance/$address/$asset")
  }

  override def nftAssetsByAddress(address: Address, limit: Long = 999L): R[Seq[NftAsset]] = mk {
    basicRequest.get(uri"$apiUri/assets/nft/${address.stringRepr}/limit/$limit")
  }

  override def assetsBalance(address: Address): R[AssetsBalancesResponse] = mk {
    basicRequest.get(uri"$apiUri/assets/balance/${address.stringRepr}")
  }

  override def broadcast(tx: Transaction): R[Unit] = mkIgnore {
    basicRequest.post(uri"$apiUri/transactions/broadcast").body(tx.toJson).contentType(MediaType.ApplicationJson)
  }

  override def transactionInfo(id: Id): R[Transaction] = mk {
    basicRequest.get(uri"$apiUri/transactions/info/$id")
  }

  override def unconfirmedTransactions: R[List[Transaction]] = mk {
    basicRequest.get(uri"$apiUri/transactions/unconfirmed")
  }

  override def unconfirmedTransactionInfo(id: Id): R[Transaction] = mk {
    basicRequest.get(uri"$apiUri/transactions/unconfirmed/info/$id")
  }

  override def currentHeightOrig: R[HeightResponse] = mk {
    basicRequest.get(uri"$apiUri/blocks/height")
  }

  override def activationStatus: R[ActivationStatusResponse] = mk {
    basicRequest.get(uri"$apiUri/activation/status")
  }

  override def connect(toNode: InetSocketAddress): R[Unit] = mkIgnore {
    basicRequest
      .post(uri"$apiUri/peers/connect")
      .body(ConnectReq(toNode.getHostName, toNode.getPort))
      .header("X-API-Key", apiKey)
      .contentType(MediaType.ApplicationJson)
  }

  override def connectedPeers: R[ConnectedPeersResponse] = mk {
    basicRequest.get(uri"$apiUri/peers/connected")
  }

  override def rollback(toHeight: Int, returnTransactionsToUtx: Boolean): R[Unit] = mkIgnore {
    basicRequest
      .post(uri"$apiUri/debug/rollback")
      .body(RollbackReq(toHeight, returnTransactionsToUtx))
      .header("X-API-Key", apiKey)
  }

  override def print(message: String): R[Unit] = mkIgnore {
    basicRequest
      .post(uri"$apiUri/debug/print")
      .body(Json.toJson(HttpMessage(message)))
      .header("X-API-Key", apiKey)
  }

}

object AsyncEnrichedNodeApi {
  type R[EntityT] = Future[EnrichedResponse[ErrorResponse, EntityT]]
}
