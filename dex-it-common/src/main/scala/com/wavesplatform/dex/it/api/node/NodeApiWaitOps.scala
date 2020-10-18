package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Functor}
import com.wavesplatform.dex.it.api.responses.node.ActivationStatusResponse
import com.wavesplatform.dex.it.fp.{CanRepeat, RepeatRequestOptions}
import im.mak.waves.transactions.Transaction
import im.mak.waves.transactions.common.Id

import scala.concurrent.duration.DurationInt

object NodeApiWaitOps {

  implicit final class Implicit[F[_]: Functor: FlatMap](val self: NodeApi[F])(implicit R: CanRepeat[F]) {

    def waitForHeight(height: Int): F[Int] =
      R.repeatUntil(self.currentHeight)(_.height >= height).map(_.height)

    def waitForHeightArise(): F[Unit] = for {
      curr <- self.currentHeight
      r <- waitForHeight(curr.height + 1)
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
