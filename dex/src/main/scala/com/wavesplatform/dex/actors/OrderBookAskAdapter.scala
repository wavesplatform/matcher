package com.wavesplatform.dex.actors

import java.util.concurrent.atomic.AtomicReference

import akka.actor._
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.model.HttpResponse
import com.wavesplatform.dex.actors.OrderBookAskAdapter.props
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.market.AggregatedOrderBookActor
import com.wavesplatform.dex.market.AggregatedOrderBookActor.Depth
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.DecimalsFormat
import com.wavesplatform.dex.model.OrderBookAggregatedSnapshot

import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag

class OrderBookAskAdapter(orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]])(implicit system: ActorSystem) {
  def getMarketStatus(assetPair: AssetPair): Future[MarketStatus] = get(assetPair, AggregatedOrderBookActor.Query.GetMarketStatus(_))

  def getAggregatedSnapshot(assetPair: AssetPair): Future[OrderBookAggregatedSnapshot] =
    get(assetPair, AggregatedOrderBookActor.Query.GetAggregatedSnapshot(_))

  def getHttpView(assetPair: AssetPair, format: DecimalsFormat, depth: Depth): Future[HttpResponse] =
    get(assetPair, AggregatedOrderBookActor.Query.GetHttpView(format, depth, _))

  private def get[M <: AggregatedOrderBookActor.Query, R](assetPair: AssetPair, message: ActorRef => M)(implicit ct: ClassTag[R]): Future[R] = {
    val r   = Promise[R]()
    val ask = system.actorOf(props(r))
    orderBooks.get().get(assetPair) match {
      case None => r.failure(new IllegalStateException("None")) // TODO
      case Some(ob) =>
        ob match {
          case Left(_)   => r.failure(new IllegalStateException("Left")) // TODO
          case Right(ob) => ob ! message(ask)
        }
    }
    r.future
  }
}

object OrderBookAskAdapter {
  private def props[T](p: Promise[T])(implicit ct: ClassTag[T]) = Props(new AskActor(p))

  private class AskActor[T](p: Promise[T])(implicit ct: ClassTag[T]) extends Actor {
    override def receive: Receive = {
      case x: T =>
        p.trySuccess(x)
        context.stop(self)

      case e: Status.Failure =>
        p.tryFailure(e.cause)
        context.stop(self)

      case x =>
        p.tryFailure(new IllegalArgumentException(s"Can't parse $x"))
        context.stop(self)
    }
  }
}
