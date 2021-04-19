package com.wavesplatform.dex.actors

import java.util.concurrent.atomic.AtomicReference

import akka.actor._
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.model.HttpResponse
import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor.{Depth, MarketStatus, Query}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.model.MatcherModel.DecimalsFormat
import com.wavesplatform.dex.model.OrderBookAggregatedSnapshot

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

// Will be removed after a migration to typed actors
class OrderBookAskAdapter(orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]], askTimeout: FiniteDuration)(
  implicit system: ActorSystem
) {
  import system.dispatcher

  type Result[T] = Future[Either[MatcherError, Option[T]]]

  def getMarketStatus(assetPair: AssetPair): Result[MarketStatus] = get[Query.GetMarketStatus, MarketStatus](assetPair, Query.GetMarketStatus(_))

  def getAggregatedSnapshot(assetPair: AssetPair): Result[OrderBookAggregatedSnapshot] =
    get[Query.GetAggregatedSnapshot, OrderBookAggregatedSnapshot](assetPair, Query.GetAggregatedSnapshot(_))

  def getHttpView(assetPair: AssetPair, format: DecimalsFormat, depth: Depth): Result[HttpResponse] =
    get[Query.GetHttpView, HttpResponse](assetPair, Query.GetHttpView(format, depth, _))

  private val default = Future.successful(Right(None))

  private def get[M <: Query, R: ClassTag](assetPair: AssetPair, message: ActorRef => M): Result[R] = orderBooks.get().get(assetPair) match {
    case None => default
    case Some(ob) =>
      ob match {
        case Left(_) => Future.successful(error.OrderBookBroken(assetPair).asLeft)
        case Right(ob) =>
          val (askRef, r) = AskActor.mk[R](askTimeout)
          ob ! message(askRef)
          r.map(_.some.asRight)
      }
  }

}
