package com.wavesplatform.dex.it.api

import java.net.{InetSocketAddress, SocketException}

import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.{Derive, FunctorK}
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{Response, SttpBackend, MonadError => _, _}
import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ConnectReq
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.it.api.domain._
import com.wavesplatform.dex.it.fp.{CanWait, FOps}
import com.wavesplatform.dex.it.json._
import com.wavesplatform.dex.it.sttp.ResponseParsers.asConfig
import com.wavesplatform.dex.it.sttp.SttpBackendOps
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.exchange.AssetPair

import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

// TODO Unit
trait NodeApi[F[_]] extends HasWaitReady[F] {

  def tryWavesBalance(address: Address): F[Either[ErrorResponse, WavesBalanceResponse]]
  def tryAssetBalance(address: Address, asset: IssuedAsset): F[Either[ErrorResponse, AssetBalanceResponse]]

  def tryBroadcast(tx: Transaction): F[Either[ErrorResponse, Unit]]
  def tryTransactionInfo(id: ByteStr): F[Either[ErrorResponse, Transaction]]

  def tryCurrentHeight: F[Either[ErrorResponse, Int]]

  def tryConfig: F[Either[ErrorResponse, Config]]

  def tryConnect(toNode: InetSocketAddress): F[Either[ErrorResponse, Unit]]
  def tryConnectedPeers: F[Either[ErrorResponse, ConnectedPeersResponse]]

  // Move
  def waitForHeight(height: Int): F[Unit]
  def waitForHeightArise(): F[Unit]
  def waitForConnectedPeer(toNode: InetSocketAddress): F[Unit]
  def waitForTransaction(id: ByteStr): F[Unit]
  def waitForTransaction(tx: Transaction): F[Unit] = waitForTransaction(tx.id.value)
}

object NodeApi {
  implicit val functorK: FunctorK[NodeApi] = Derive.functorK[NodeApi]

  def apply[F[_]](apiKey: String,
                  host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): NodeApi[F] =
    new NodeApi[F] {
      private val ops = FOps[F]
      import ops._

      private val sttpOps = SttpBackendOps[F, ErrorResponse]
      import sttpOps._

      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}"

      override def tryWavesBalance(address: Address): F[Either[ErrorResponse, WavesBalanceResponse]] =
        tryParseJson(sttp.get(uri"$apiUri/addresses/balance/$address"))

      override def tryAssetBalance(address: Address, asset: IssuedAsset): F[Either[ErrorResponse, AssetBalanceResponse]] =
        tryParseJson(sttp.get(uri"$apiUri/assets/balance/$address/${AssetPair.assetIdStr(asset)}"))

      override def tryBroadcast(tx: Transaction): F[Either[ErrorResponse, Unit]] = tryUnit(sttp.post(uri"$apiUri/transactions/broadcast").body(tx))

      override def tryTransactionInfo(id: ByteStr): F[Either[ErrorResponse, Transaction]] = tryParseJson(sttp.get(uri"$apiUri/transactions/info/$id"))

      override def tryCurrentHeight: F[Either[ErrorResponse, Int]] =
        tryParseJson[HeightResponse](sttp.get(uri"$apiUri/blocks/height")).map(_.map(_.height))

      override def tryConfig: F[Either[ErrorResponse, Config]] = tryParse(sttp.get(uri"$apiUri/blocks/height").response(asConfig))

      override def tryConnect(toNode: InetSocketAddress): F[Either[ErrorResponse, Unit]] = tryUnit {
        sttp.post(uri"$apiUri/peers/connect").body(ConnectReq(toNode.getHostName, toNode.getPort)).header("X-API-Key", apiKey)
      }

      override def tryConnectedPeers: F[Either[ErrorResponse, ConnectedPeersResponse]] = tryParseJson(sttp.get(uri"$apiUri/peers/connected"))

      override def waitForConnectedPeer(toNode: InetSocketAddress): F[Unit] = {
        val hostName = toNode.getHostName
        repeatUntil(tryConnectedPeers, 1.second) {
          case Right(x) => x.peers.exists(p => p.address.contains(hostName))
          case _        => false
        }.map(_ => ())
      }

      override def waitForTransaction(id: ByteStr): F[Unit] = repeatUntil(tryTransactionInfo(id), 1.second)(_.isRight).map(_ => ())

      override def waitForHeightArise(): F[Unit] =
        tryCurrentHeight
          .flatMap {
            case Right(origHeight) =>
              repeatUntil(tryCurrentHeight, 1.second) {
                case Right(x) => x > origHeight
                case _        => false
              }.map(_ => ())
            case Left(_) => waitForHeightArise()
          }

      override def waitForHeight(height: Int): F[Unit] =
        FOps[F]
          .repeatUntil(tryCurrentHeight, 1.second) {
            case Right(x) => x >= height
            case _        => false
          }
          .map(_ => ())

      override def waitReady: F[Unit] = {
        val req = sttp.get(uri"$apiUri/blocks/height").mapResponse(_ => ())

        def loop(): F[Response[Unit]] = M.handleErrorWith(httpBackend.send(req)) {
          case _: SocketException => W.wait(1.second).flatMap(_ => loop())
          case NonFatal(e)        => M.raiseError(e)
        }

        repeatUntil(loop(), 1.second)(_.code == StatusCodes.Ok).map(_ => ())
      }
    }
}
