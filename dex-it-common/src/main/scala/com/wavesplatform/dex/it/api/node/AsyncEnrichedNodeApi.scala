package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress
import java.util.UUID

import com.softwaremill.sttp._
import com.softwaremill.sttp.playJson._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.it.api.node.AsyncEnrichedNodeApi._
import com.wavesplatform.dex.it.api.responses.node._
import com.wavesplatform.dex.it.api.{AsyncEnriched, EnrichedResponse}
import im.mak.waves.transactions.Transaction
import im.mak.waves.transactions.common.Id
import play.api.libs.json.{JsSuccess, Reads}

import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedNodeApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends NodeApi[AsyncEnriched] {

  override def wavesBalanceOrig(address: Address): AsyncEnriched[WavesBalanceResponse] = mk {
    sttp.get(uri"$apiUri/addresses/balance/$address")
  }

  override def assetBalanceOrig(address: Address, asset: Asset.IssuedAsset): AsyncEnriched[AssetBalanceResponse] = mk {
    sttp.get(uri"$apiUri/assets/balance/$address/$asset")
  }

  override def broadcast(tx: Transaction): AsyncEnriched[Unit] = mkIgnore {
    sttp.post(uri"$apiUri/transactions/broadcast").body(tx.toJson)
  }

  override def transactionInfo(id: Id): AsyncEnriched[Transaction] = mk {
    sttp.get(uri"$apiUri/transactions/info/$id")
  }

  override def currentHeightOrig: AsyncEnriched[HeightResponse] = mk {
    sttp.get(uri"$apiUri/blocks/height")
  }

  override def activationStatus: AsyncEnriched[ActivationStatusResponse] = mk {
    sttp.get(uri"$apiUri/activation/status")
  }

  override def connect(toNode: InetSocketAddress): AsyncEnriched[Unit] = mkIgnore {
    sttp.post(uri"$apiUri/peers/connect").body(ConnectReq(toNode.getHostName, toNode.getPort)).header("X-API-Key", apiKey)
  }

  override def connectedPeers: AsyncEnriched[ConnectedPeersResponse] = mk {
    sttp.get(uri"$apiUri/peers/connected")
  }

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

  def mk[T: Reads](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map {
      EnrichedResponse.AsJson[T](_)
    }

  def mkHocon[T](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map(EnrichedResponse.AsHocon[T])

  def mkIgnore(req: Request[String, Nothing]): AsyncEnriched[Unit] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map(EnrichedResponse.Ignore)

}

// TODO
object AsyncEnrichedNodeApi {

  implicit val transactionReads: Reads[Transaction] = Reads { js =>
    JsSuccess(Transaction.fromJson(js.toString()))
  }

}
