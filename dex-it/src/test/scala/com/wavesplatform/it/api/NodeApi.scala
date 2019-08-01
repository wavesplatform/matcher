package com.wavesplatform.it.api

import cats.Id
import com.google.protobuf.empty.Empty
import com.wavesplatform.api.grpc._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.docker.NodeContainer
import com.wavesplatform.it.util.{GlobalTimer, TimerExt}
import com.wavesplatform.transaction
import io.grpc.stub.StreamObserver

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

trait NodeApi[F[_]] {
  def broadcast(tx: transaction.Transaction): F[Unit]
  def waitForTransaction(id: ByteStr): F[Unit]
  def waitForHeightArise(): F[Unit]
}

object NodeApi {
  implicit final class FutureTOps(val self: Future.type) extends AnyVal {
    def repeatUntil[T](f: => Future[T], delay: FiniteDuration)(pred: T => Boolean)(implicit ec: ExecutionContext): Future[T] =
      f.flatMap { x =>
        if (pred(x)) Future.successful(x)
        else GlobalTimer.instance.sleep(delay).flatMap(_ => repeatUntil(f, delay)(pred))
      }
  }

  implicit final class FutureOps[T](val self: Future[T]) extends AnyVal {
    def toUnit(implicit ec: ExecutionContext): Future[Unit] = self.map(_ => ())
  }

  def async(node: NodeContainer)(implicit ec: ExecutionContext): NodeApi[Future] = new NodeApi[Future] {
    private val txApi     = TransactionsApiGrpc.stub(node.grpcChannel)
    private val blocksApi = BlocksApiGrpc.stub(node.grpcChannel)

    override def broadcast(tx: transaction.Transaction): Future[Unit] = txApi.broadcast(tx.toPB).toUnit

    override def waitForTransaction(id: ByteStr): Future[Unit] = {
      val r = Promise[Unit]()
      txApi.getStatuses(
        TransactionsByIdRequest(Seq(id)),
        new StreamObserver[TransactionStatus] {
          override def onNext(value: TransactionStatus): Unit = if (value.status.isConfirmed) r.success(())
          override def onError(t: Throwable): Unit            = r.failure(t)
          override def onCompleted(): Unit                    = {}
        }
      )
      r.future
    }

    override def waitForHeightArise(): Future[Unit] = {
      currentHeight.flatMap { origHeight =>
        Future.repeatUntil(currentHeight, 1.second)(_ > origHeight).toUnit
      }
    }

    def currentHeight: Future[Int] = blocksApi.getCurrentHeight(new Empty()).map(_.value)
  }

  def sync(async: NodeApi[Future]): NodeApi[Id] = new NodeApi[Id] {
    override def broadcast(tx: transaction.Transaction): Id[Unit] = wait(async.broadcast(tx))

    override def waitForTransaction(id: ByteStr): Id[Unit] = wait(async.waitForTransaction(id))

    override def waitForHeightArise(): Id[Unit] = wait(async.waitForHeightArise())

    private def wait[T](x: Future[T]): T = Await.result(x, 1.minute)
  }
}
