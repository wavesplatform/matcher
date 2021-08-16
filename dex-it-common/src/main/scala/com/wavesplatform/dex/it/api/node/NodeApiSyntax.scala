package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Functor}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.it.api.responses.node.ActivationStatusResponse
import com.wavesplatform.dex.it.fp.{CanRepeat, RepeatRequestOptions}
import com.wavesplatform.transactions.Transaction
import com.wavesplatform.transactions.common.Id

import scala.concurrent.duration.DurationInt

object NodeApiSyntax {

  implicit final class Ops[F[_]: Functor: FlatMap](val self: NodeApi[F])(implicit R: CanRepeat[F]) {

    def balance(address: Address, asset: Asset): F[Long] = asset match {
      case asset: IssuedAsset => self.assetBalance(address, asset)
      case Asset.Waves => self.wavesBalance(address)
    }

    def wavesBalance(address: Address): F[Long] = self.wavesBalanceOrig(address).map(_.balance)
    def assetBalance(address: Address, asset: IssuedAsset): F[Long] = self.assetBalanceOrig(address, asset).map(_.balance)

    def currentHeight: F[Int] = self.currentHeightOrig.map(_.height)

    def waitForHeight(height: Int): F[Int] = R.repeatUntil(currentHeight)(_ >= height)

    def waitForHeightArise(): F[Int] = for {
      curr <- currentHeight
      r <- waitForHeight(curr + 1)
    } yield r

    def waitForConnectedPeer(toNode: InetSocketAddress): F[Unit] = {
      val hostName = toNode.getHostName
      R.repeatUntil(self.connectedPeers)(_.peers.exists(p => p.address.contains(hostName))).map(_ => ())
    }

    def waitForTransaction(tx: Transaction): F[Unit] = waitForTransaction(tx.id())
    def waitForTransaction(id: Id): F[Unit] = R.repeatUntil(self.transactionInfo(id))(_ => true).map(_ => ())

    def waitForActivationStatus(f: ActivationStatusResponse => Boolean): F[ActivationStatusResponse] =
      R.repeatUntil(self.activationStatus, RepeatRequestOptions(1.second, 180))(f)

  }

}
